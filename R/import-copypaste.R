
#' import-copypaste UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList textAreaInput
#' @importFrom data.table fread
mod_import_copypaste_ui <- function(id){
  ns <- NS(id)
  tagList(
    html_dependency_datamods(),
    textAreaInput(
      inputId = ns("data_pasted"),
      label = "Paste Code",
      height = "500px"
    ),
    
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b("Nothing pasted yet!"),
        "Please copy and paste some data in the above dialogue box.",
        dismissible = TRUE
      )
    ),
    
    tags$div(
      id = ns("validate-button"),
      style = "margin-top: 20px;",
      actionButton(
        inputId = ns("validate"),
        label = "Import selected data",
        icon = icon("arrow-circle-right"),
        width = "100%",
        disabled = "disabled",
        class = "btn-primary"
      )
    )
    
    
    
  )
}

#' import-copypaste Server Function
#'
#' @noRd 
mod_import_copypaste_server <- function(input, output, session,
                                        default_data = NULL,
                                        update_data = c("button", "always")){
  
  ns <- session$ns
  update_data <- match.arg(update_data)
  imported_data <- reactiveValues(data = default_data)
  temporary_data <- reactiveValues(data = default_data)
  
  observeEvent(input$data_pasted, {
    
    
    imported <- try(data.table::fread(input = input$data_pasted), silent = TRUE)
    
    
    if (inherits(imported, "try-error") || NROW(imported) < 1) {
      
      toggle_widget(inputId = ns("validate"), enable = FALSE)
      
      insert_alert(
        selector = ns("import"),
        status = "danger",
        tags$b(icon("exclamation-triangle"), "Ooops"), "Something went wrong..."
      )
      
    } else {
      
      toggle_widget(inputId = ns("validate"), enable = TRUE)
      
      if (identical(update_data, "button")) {
        success_message <- tagList(
          tags$b(icon("check"), "Data ready to be imported!"),
          sprintf(
            "%s obs. of %s variables imported",
            nrow(imported), ncol(imported)
          )
        )
      } else {
        success_message <- tagList(
          tags$b(icon("check"), "Data successfully imported!"),
          sprintf(
            "%s obs. of %s variables imported",
            nrow(imported), ncol(imported)
          )
        )
      }
      
      insert_alert(
        selector = ns("import"),
        status = "success",
        success_message
      )
      
      temporary_data$data <- imported
    }
    
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$validate, {
    imported_data$data <- temporary_data$data
  })
  
  
  if (identical(update_data, "button")) {
    return(list(
      data = reactive(imported_data$data)
    ))
  } else {
    return(list(
      data = reactive(temporary_data$data)
    ))
  }
}
