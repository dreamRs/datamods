
#' @title import data with a googlesheet link
#'
#' @description Let user paste link to a Google sheet then import the data.
#'
#' @param id Module's ID
#' 
#' @return
#'  * UI: HTML tags that can be included in shiny's UI
#'  * Server: a \code{list} with one slot:
#'    + **data**: a \code{reactive} function returning the selected \code{data.frame}.
#'
#' @export
#' @name import-googlesheets
#'
#' @importFrom shiny NS tagList 
#' 
#' @example examples/googlesheets.R
import_googlesheets_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    html_dependency_datamods(),
    tags$h2("GoogleSheets"),
    actionButton(
      inputId = ns("sign_in"),
      label = "Sign-in to Google",
    ), 
    br(),
    tags$div(
      id = ns("signin-placeholder"),
      alert(
        id = ns("signin-result"),
        status = "info",
        tags$b("Status:"),
        "Not signed in.",
        dismissible = TRUE
      )
    ),
    tagAppendAttributes(
      textAreaInput(
        inputId = ns("link"),
        label = "Paste link here:",
        height = "100px",
        width = "100%",
        resize = "none"
      ),
      class = "shiny-input-container-inline"
    ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b("Nothing pasted yet!"),
        "Please paste a valid googlesheets link in the dialog box above.",
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

#' @importFrom shiny callModule
#'

import_googlesheets_server <- function(id,
                                       default_data = NULL,
                                       update_data = c("button", "always")) {
  
  callModule(
    module = import_googlesheets,
    id = id,
    default_data = default_data,
    update_data = update_data
  )
}


#' @importFrom googlesheets4 range_read gs4_auth gs4_has_token

import_googlesheets <- function(input, output, session,
                                default_data = NULL,
                                update_data = c("button", "always")) {
  ns <- session$ns
  update_data <- match.arg(update_data)
  imported_data <- reactiveValues(data = default_data)
  temporary_data <- reactiveValues(data = default_data)
  
  token <- eventReactive(input$sign_in, {
    #req(!googlesheets4::gs4_has_token())
    googlesheets4::gs4_auth(cache = FALSE)
    googlesheets4::gs4_has_token()
  })
  
  
  observeEvent(token(),{
    #print(class(token()))
    if(googlesheets4::gs4_has_token()){
      insert_alert(
        selector = ns("signin"),
        status = "success",
        tags$b("Status:"),
        "Signed in!"
      )
    } else {
      insert_alert(
        selector = ns("signin"),
        status = "info",
        tags$b("Status:"),
        "Not signed in."
      )
    }
  })
  
  
  if (identical(update_data, "always")) {
    removeUI(selector = paste0("#", ns("validate-button")))
  }
  
  observeEvent(input$link, {
    imported <- try(googlesheets4::range_read(input$link), silent = TRUE)
    
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

## To be copied in the UI
# mod_import_googlesheets_ui("import_googlesheets_ui_1")

## To be copied in the server
# callModule(mod_import_googlesheets_server, "import_googlesheets_ui_1")

