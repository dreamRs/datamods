
#' @title Import data from a file
#'
#' @description Let user upload a file and import data
#'
#' @param id Module's ID.
#' @param title Module's title, if \code{TRUE} use the default title,
#'  use \code{NULL} for no title or a \code{shiny.tag} for a custom one.
#'
#' @eval doc_return_import()
#'
#' @export
#'
#' @name import-file
#'
#' @importFrom shiny NS fileInput tableOutput actionButton icon
#' @importFrom htmltools tags tagAppendAttributes
#' @importFrom shinyWidgets pickerInput numericInputIcon textInputIcon dropMenu
#'
#' @example examples/from-file.R
import_file_ui <- function(id, title = TRUE) {

  ns <- NS(id)

  if (isTRUE(title)) {
    title <- tags$h4(
      i18n("Import a file"),
      class = "datamods-title"
    )
  }

  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    title,
    tags$div(
      class = "datamods-file-import",
      tags$div(
        fileInput(
          inputId = ns("file"),
          label = i18n("Upload a file:"),
          buttonLabel = i18n("Browse..."),
          placeholder = i18n("No file selected"),
          accept = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav"),
          width = "100%"
        )
      ),
      tags$div(
        dropMenu(
          placement = "bottom-end",
          actionButton(
            inputId = ns("settings"),
            label = NULL,
            icon = icon("gear"),
            class = "btn-block",
            style = "margin-top: 25px;"
          ),
          numericInputIcon(
            inputId = ns("skip_rows"),
            label = i18n("Number of rows to skip before reading data:"),
            value = 0,
            min = 0,
            icon = list("n =")
          ),
          textInputIcon(
            inputId = ns("dec"),
            label = i18n("Decimal separator:"),
            value = ".",
            icon = list("0.00")
          ),
          textInputIcon(
            inputId = ns("encoding"),
            label = i18n("Encoding:"),
            value = "UTF-8",
            icon = icon("font")
          )
        )
      )
    ),
    tags$div(
      class = "hidden",
      id = ns("sheet-container"),
      pickerInput(
        inputId = ns("sheet"),
        label = i18n("Select sheet to import:"),
        choices = NULL,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b(i18n("No file selected:")),
        i18n("You can import .rds, .txt, .csv, .xls, .xlsx, .sas7bdat, .sav, ..."),
        dismissible = TRUE
      )
    ),
    tagAppendAttributes(
      tableOutput(outputId = ns("table")),
      class = "datamods-table-container"
    ),
    uiOutput(
      outputId = ns("container_confirm_btn"),
      style = "margin-top: 20px;"
    )
  )
}


#' @param btn_show_data Display or not a button to display data in a modal window if import is successful.
#' @param trigger_return When to update selected data:
#'  \code{"button"} (when user click on button) or
#'  \code{"change"} (each time user select a dataset in the list).
#' @param return_class Class of returned data: \code{data.frame}, \code{data.table} or \code{tbl_df} (tibble).
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom htmltools tags tagList
#' @importFrom shiny reactiveValues reactive observeEvent removeUI req renderTable
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom readxl excel_sheets
#' @importFrom rio import
#' @importFrom tools file_ext
#' @importFrom utils head
#'
#' @rdname import-file
import_file_server <- function(id,
                               btn_show_data = TRUE,
                               trigger_return = c("button", "change"),
                               return_class = c("data.frame", "data.table", "tbl_df")) {

  trigger_return <- match.arg(trigger_return)

  module <- function(input, output, session) {

    ns <- session$ns
    imported_rv <- reactiveValues(data = NULL, name = NULL)
    temporary_rv <- reactiveValues(data = NULL, name = NULL, status = NULL)

    output$container_confirm_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        button_import()
      }
    })

    observeEvent(input$file, {
      if (isTRUE(is_excel(input$file$datapath))) {
        updatePickerInput(
          session = session,
          inputId = "sheet",
          choices = readxl::excel_sheets(input$file$datapath)
        )
        showUI(paste0("#", ns("sheet-container")))
      } else {
        hideUI(paste0("#", ns("sheet-container")))
      }
    })

    observeEvent(list(
      input$file,
      input$sheet,
      input$skip_rows,
      input$dec,
      input$encoding
    ), {
      req(input$file)
      req(input$skip_rows)
      if (is_excel(input$file$datapath)) {
        req(input$sheet)
        imported <- try(rio::import(
          file = input$file$datapath,
          which = input$sheet,
          skip = input$skip_rows
        ), silent = TRUE)
      } else {
        imported <- try(rio::import(
          file = input$file$datapath,
          skip = input$skip_rows,
          dec = input$dec,
          encoding = input$encoding
        ), silent = TRUE)
      }

      if (inherits(imported, "try-error") || NROW(imported) < 1) {

        toggle_widget(inputId = "confirm", enable = FALSE)
        insert_error()
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
            btn_show_data = btn_show_data,
            extra = i18n("First five rows are shown below:")
          )
        )
        temporary_rv$status <- "success"
        temporary_rv$data <- imported
        temporary_rv$name <- input$file$name

      }
    }, ignoreInit = TRUE)

    observeEvent(input$see_data, {
      show_data(temporary_rv$data, title = i18n("Imported data"))
    })

    output$table <- renderTable({
      req(temporary_rv$data)
      data <- head(temporary_rv$data, 5)
      classes <- get_classes(data)
      classes <- sprintf("<span style='font-style: italic; font-weight: normal; font-size: small;'>%s</span>", classes)
      names(data) <- paste(names(data), classes, sep = "<br>")
      data
    }, striped = TRUE, bordered = TRUE, sanitize.colnames.function = identity, spacing = "xs")

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

# utils -------------------------------------------------------------------

is_excel <- function(path) {
  isTRUE(tools::file_ext(path) %in% c("xls", "xlsx"))
}

