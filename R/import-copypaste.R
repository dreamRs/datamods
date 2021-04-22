
#' @title Import data with copy & paste
#'
#' @description Let the user copy data from Excel or text file then paste it into a text area to import it.
#'
#' @param id Module's ID.
#' @param title Module's title, if \code{TRUE} use the default title,
#'  use \code{NULL} for no title or a \code{shiny.tag} for a custom one.
#'
#' @eval doc_return_import()
#'
#' @export
#'
#' @name import-copypaste
#'
#' @importFrom shiny NS icon textAreaInput actionButton
#' @importFrom htmltools tags tagAppendAttributes
#'
#' @example examples/copypaste.R
import_copypaste_ui <- function(id, title = TRUE) {

  ns <- NS(id)

  if (isTRUE(title)) {
    title <- tags$h4(
      i18n("Copy & paste data"),
      class = "datamods-title"
    )
  }

  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    title,
    tagAppendAttributes(
      textAreaInput(
        inputId = ns("data_pasted"),
        label = i18n("Paste data here:"),
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
        tags$b(i18n("Nothing pasted yet!")),
        i18n("Please copy and paste some data in the dialog box above."),
        dismissible = TRUE
      )
    ),
    uiOutput(
      outputId = ns("container_valid_btn"),
      style = "margin-top: 20px;"
    )
  )
}


#' @param btn_show_data Display or not a button to display data in a modal window if import is successful.
#' @param trigger_return When to update selected data:
#'  \code{"button"} (when user click on button) or
#'  \code{"change"} (each time user select a dataset in the list).
#' @param return_class Class of returned data: \code{data.frame}, \code{data.table} or \code{tbl_df} (tibble).
#' @param reset A `reactive` function that when triggered resets the data.
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom data.table fread
#' @importFrom shiny reactiveValues observeEvent removeUI reactive
#' @importFrom htmltools tags tagList
#'
#' @rdname import-copypaste
import_copypaste_server <- function(id,
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

    output$container_valid_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        button_import()
      }
    })

    observeEvent(input$data_pasted, {
      req(input$data_pasted)
      imported <- try(data.table::fread(text = input$data_pasted), silent = TRUE)

      if (inherits(imported, "try-error") || NROW(imported) < 1) {
        toggle_widget(inputId = "confirm", enable = FALSE)
        insert_error()
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
        name = reactive("clipboard_data"),
        data = reactive(as_out(imported_rv$data, return_class))
      ))
    } else {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive("clipboard_data"),
        data = reactive(as_out(temporary_rv$data, return_class))
      ))
    }
  }

  moduleServer(
    id = id,
    module = module
  )
}



