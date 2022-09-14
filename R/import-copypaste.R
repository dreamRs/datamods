
#' @title Import data with copy & paste
#'
#' @description Let the user copy data from Excel or text file then paste it into a text area to import it.
#'
#' @inheritParams import-globalenv
#' @param name_field Show or not a field to add a name to data (that is returned server-side).
#'
#' @template module-import
#'
#' @export
#'
#' @name import-copypaste
#'
#' @importFrom shiny NS icon textAreaInput actionButton textInput
#' @importFrom htmltools tags tagAppendAttributes
#'
#' @example examples/from-copypaste.R
import_copypaste_ui <- function(id, title = TRUE, name_field = TRUE) {

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
   if (isTRUE(name_field)) {
     textInput(
       inputId = ns("name"),
       label = NULL,
       placeholder = i18n("Add a label to data"),
       width = "100%"
     )
   },
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


#' @inheritParams import_globalenv_server
#' @param fread_args `list` of additional arguments to pass to [data.table::fread()] when reading data.
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom data.table fread
#' @importFrom shiny reactiveValues observeEvent removeUI reactive
#' @importFrom htmltools tags tagList
#' @importFrom rlang %||%
#'
#' @rdname import-copypaste
import_copypaste_server <- function(id,
                                    btn_show_data = TRUE,
                                    show_data_in = c("popup", "modal"),
                                    trigger_return = c("button", "change"),
                                    return_class = c("data.frame", "data.table", "tbl_df"),
                                    reset = reactive(NULL),
                                    fread_args = list()) {

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
      fread_args$tex <- input$data_pasted
      imported <- try(rlang::exec(data.table::fread, !!!fread_args), silent = TRUE)

      if (inherits(imported, "try-error") || NROW(imported) < 1) {
        toggle_widget(inputId = "confirm", enable = FALSE)
        insert_error(mssg = i18n(attr(imported, "condition")$message))
        temporary_rv$status <- "error"
        temporary_rv$data <- NULL
        temporary_rv$name <- NULL
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

    observeEvent(input$name, {
      temporary_rv$name <- if (isTruthy(input$name)) {
        input$name
      } else {
        "clipboard_data"
      }
    })

    observeEvent(input$see_data, {
      show_data(temporary_rv$data, title = i18n("Imported data"), type = show_data_in)
    })

    observeEvent(input$confirm, {
      imported_rv$data <- temporary_rv$data
      imported_rv$name <- temporary_rv$name
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



