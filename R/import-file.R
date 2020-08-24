

#' import-file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyjs useShinyjs show hide hidden
import_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    html_dependency_datamods(),
    shinyjs::useShinyjs(),
    tags$h2("Import a dataset"),
    fileInput(
      inputId = ns("file"), 
      label = "Choose a file:", 
      accept = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav"),
      width = "100%"
    ),
    shinyjs::hidden(selectInput(ns("sheet"), "Sheet", choices = 1, selected = 1)),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b("No file"), "Import .rds, .txt, .csv, .xls, .xlsx, .sas7bdat, .sav, ...",
        dismissible = TRUE
      )
    ),
    tags$div(
      id = ns("validate-button"),
      style = "margin-top: 20px;",
      actionButton(
        inputId = ns("validate"),
        label = "Import data",
        icon = icon("arrow-circle-right"),
        width = "100%",
        disabled = "disabled",
        class = "btn-primary"
      )
    )
  )
}

#' import-file Server Function
#' 

import_file_server <- function(id,
                               default_data = NULL,
                               update_data = c("button", "always")) {
  callModule(
    module = import_file,
    id = id,
    default_data = default_data,
    update_data = update_data
  )
}

import_file <- function(input, output, session,
                        default_data = NULL,
                        update_data = c("button", "always")) {
  ns <- session$ns
  update_data <- match.arg(update_data)
  imported_data <- reactiveValues(data = default_data)
  temporary_data <- reactiveValues(data = default_data)
  
  if (identical(update_data, "always")) {
    removeUI(selector = paste0("#", ns("validate-button")))
  }
  
  is_excel <- reactive({
    req(input$file)
    file_extension = tools::file_ext(input$file$name)
    if (file_extension %in% c("xls", "xlsx")) TRUE
    else FALSE
  })
  
  observeEvent(is_excel(), {
    req(is_excel())
    sheets <- length(readxl::excel_sheets(input$file$datapath))
    updateSelectInput(session = session, inputId = "sheet", label = "Sheet",
                      choices = 1:sheets, selected = 1)
    shinyjs::show("sheet", anim = T, animType = "slide")
  })
  
  observeEvent({
    input$file
    input$sheet
  }, {
    if(isFALSE(is_excel())) shinyjs::hide("sheet", anim = T, animType = "fade")

    imported <- try(rio::import(file = input$file$datapath, which = as.numeric(input$sheet)), silent = TRUE)
    
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



