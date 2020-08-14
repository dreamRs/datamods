
#' @title Import data with copy & paste
#'
#' @description Let the user copy data from Excel or text file then paste it into a text area to import it.
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
#' @importFrom htmltools tagList tags tagAppendAttributes
#'
#' @example examples/copypaste.R
import_copypaste_ui <- function(id) {

  ns <- NS(id)

  tagList(
    html_dependency_datamods(),
    tags$h2("Copy & paste data"),
    tagAppendAttributes(
      textAreaInput(
        inputId = ns("data_pasted"),
        label = "Paste data here:",
        height = "300px",
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
        "Please copy and paste some data in the dialog box above.",
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
import_copypaste_server <- function(id,
                                    default_data = NULL,
                                    update_data = c("button", "always")) {
  callModule(
    module = import_copypaste,
    id = id,
    default_data = default_data,
    update_data = update_data
  )
}


#' @importFrom data.table fread
#' @importFrom shiny reactiveValues observeEvent removeUI reactive
#' @importFrom htmltools tags
import_copypaste <- function(input, output, session,
                             default_data = NULL,
                             update_data = c("button", "always")) {

  ns <- session$ns
  update_data <- match.arg(update_data)
  imported_data <- reactiveValues(data = default_data)
  temporary_data <- reactiveValues(data = default_data)

  if (identical(update_data, "always")) {
    removeUI(selector = paste0("#", ns("validate-button")))
  }

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
