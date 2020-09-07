

#' @title Import data from a database
#'
#' @description 
#'
#' @param id Module's ID.
#'
#' @return
#'  * UI: HTML tags that can be included in shiny's UI
#'  * Server: a \code{list} with one slot:
#'    + **data**: a \code{reactive} function returning the selected \code{data.frame}.
#'
#' @export
#'
#' @name import-copypaste
#'
#' @importFrom shiny NS icon textAreaInput actionButton
#' @importFrom htmltools tags tagAppendAttributes
#'
#' @example examples/copypaste.R
import_database_ui <- function(id) {
  
  ns <- NS(id)
  
  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    tags$h2("Database connection"),
    tags$br(),
    pickerInput(
      inputId = ns("tables"),
      label = "Select a table",
      choices = "Waiting for connection"
    ),
    # tagAppendAttributes(
    #   textAreaInput(
    #     inputId = ns("data_pasted"),
    #     label = "Paste data here:",
    #     height = "300px",
    #     width = "100%",
    #     resize = "none"
    #   ),
    #   class = "shiny-input-container-inline"
    # ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b("No connection found!"),
        "Please make sure you have an active connnection",
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


#' @param default_data Default \code{data.frame} to use.
#' @param update_data When to update selected data:
#'  \code{"button"} (when user click on button) or
#'  \code{"always"} (each time user select a dataset in the list).
#'
#' @export
#'
#' @importFrom shiny callModule
#'
#' @rdname import-copypaste
import_database_server <- function(id,
                                   default_data = NULL,
                                   update_data = c("button", "always")) {
  
  con_char <- search_obj(what = "SQLiteConnection")
  con <- try(eval(as.symbol(con_char)), silent = TRUE)
  callModule(
    module = import_database,
    id = id,
    default_data = default_data,
    update_data = update_data,
    connection = con
  )
}


#' @importFrom data.table fread
#' @importFrom shiny reactiveValues observeEvent removeUI reactive
#' @importFrom htmltools tags tagList
import_database <- function(input, output, session, 
                            default_data = NULL,
                            update_data = c("button", "always"),
                            connection) {
  
  ns <- session$ns
  update_data <- match.arg(update_data)
  imported_data <- reactiveValues(data = default_data)
  temporary_data <- reactiveValues(data = default_data)
  
  observeEvent(connection, {
    if (!inherits(connection, "try-error")) {
      updatePickerInput(
        session = session,
        inputId = "tables",
        choices = DBI::dbListTables(connection)
      )
    }
  })
  
  
  
  if (identical(update_data, "always")) {
    removeUI(selector = paste0("#", ns("validate-button")))
  }
  
  observeEvent(input$tables, {
    
    imported <- try(DBI::dbReadTable(connection, input$tables), silent = TRUE)
    
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
          tags$b(icon("check"), "Connection found, data ready to be imported!"),
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
      success_message <- tagList(
        success_message,
        tags$br(),
        actionLink(
          inputId = ns("see_data"),
          label = "click to see data",
          icon = icon("hand-o-right")
        )
      )
      
      insert_alert(
        selector = ns("import"),
        status = "success",
        success_message
      )
      
      temporary_data$data <- imported
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$see_data, {
    show_data(temporary_data$data)
  })
  
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
