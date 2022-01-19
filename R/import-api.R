#' @title Import data from API providing a flattened JSON
#'
#' @description Let user paste link to a JSON then import the data.
#'
#' @inheritParams import-globalenv
#'
#' @eval doc_return_import()
#'
#' @export
#' @name import-api
#' 
#' @importFrom htmltools tags
#' 
#' @example examples/from-api.R
import_api_ui <- function(id, title = TRUE) {
  
  ns <- shiny::NS(id)
  
  if (isTRUE(title)) {
    title <- tags$h4(
      i18n("Import JSON API"),
      class = "datamods-title"
    )
  }
  
  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    title,
    shinyWidgets::textInputIcon(
      inputId = ns("link"),
      label = i18n("Enter URL to JSON data:"),
      icon = phosphoricons::ph("link"),
      width = "100%"
    ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b(i18n("Nothing pasted yet!")),
        i18n("Please paste a valid link in the dialog box above."),
        dismissible = TRUE
      )
    ),
    shiny::uiOutput(
      outputId = ns("container_confirm_btn"),
      style = "margin-top: 20px;"
    )
  )
}

#' @inheritParams import_globalenv_server
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom shiny reactiveValues observeEvent removeUI reactive req
#' @importFrom htmltools tags tagList
#'
#' @rdname import-api
import_api_server <- function(id,
                                       btn_show_data = TRUE,
                                       trigger_return = c("button", "change"),
                                       return_class = c("data.frame", "data.table", "tbl_df"),
                                       reset = reactive(NULL)) {
  
  trigger_return <- match.arg(trigger_return)
  
  module <- function(input, output, session) {
    
    ns <- session$ns
    imported_rv <- reactiveValues(data = NULL, name = NULL)
    temporary_rv <- reactiveValues(data = NULL, name = NULL, status = NULL)
    
    observeEvent(reset(), {
      temporary_rv$data <- NULL
      temporary_rv$name <- NULL
      temporary_rv$status <- NULL
    })
    
    output$container_confirm_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        button_import()
      }
    })
    
    observeEvent(input$trigger, {
      if (identical(trigger_return, "change")) {
        hideUI(selector = paste0("#", ns("confirm-button")))
      }
    })
    
    observeEvent(input$link, {
      req(input$link)
      
      imported <- try(read_json(input$link), silent = TRUE)
      if (inherits(imported, "try-error") || NROW(imported) < 1) {
        toggle_widget(inputId = "confirm", enable = FALSE)
        insert_error(mssg = i18n(attr(imported, "condition")$message))
        temporary_rv$status <- "error"
        temporary_rv$data <- NULL
      } else {
        toggle_widget(inputId = "confirm", enable = TRUE)
        insert_alert(
          selector = ns("import"),
          status = "success",
          make_success_alert(
            imported,
            trigger_return = trigger_return,
            btn_show_data = btn_show_data
          )
        )
        temporary_rv$status <- "success"
        temporary_rv$data <- imported
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$see_data, {
      show_data(temporary_rv$data, title = i18n("Imported data"))
    })
    
    observeEvent(input$confirm, {
      imported_rv$data <- temporary_rv$data
    })
    
    if (identical(trigger_return, "button")) {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(imported_rv$name),
        data = reactive(as_out(imported_rv$data, return_class))
      ))
    } else {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(temporary_rv$name),
        data = reactive(as_out(temporary_rv$data, return_class))
      ))
    }
  }
  
  moduleServer(
    id = id,
    module = module
  )
}


# Utils -------------------------------------------------------------------

read_json <- function(url) {
  resp <- httr::GET(url)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json.", call. = FALSE)
  }
  
  parsed <- httr::content(resp, "text") %>%
    jsonlite::fromJSON()
  
  if (httr::http_error(resp)) {
    stop("API request failed.", call. = FALSE)
  }
  
  if (!is.data.frame(parsed) | !all(vapply(parsed, is.atomic, logical(1)))) {
    stop("JSON data is not in flat format.", call. = FALSE)
  }
  
  return(parsed)
}