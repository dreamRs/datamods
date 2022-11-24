
#' @title Import data from a file
#'
#' @description Let user upload a file and import data
#'
#' @inheritParams import-globalenv
#' @param preview_data Show or not a preview of the data under the file input.
#' @param file_extensions File extensions accepted by [shiny::fileInput()], can also be MIME type.
#'
#' @template module-import
#'
#' @export
#'
#' @name import-file
#'
#' @importFrom shiny NS fileInput tableOutput actionButton icon
#' @importFrom htmltools tags tagAppendAttributes css
#' @importFrom shinyWidgets pickerInput numericInputIcon textInputIcon dropMenu
#'
#' @example examples/from-file.R
import_file_ui <- function(id,
                           title = TRUE,
                           preview_data = TRUE,
                           file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav")) {

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
          accept = file_extensions,
          width = "100%"
        )
      ),
      tags$div(
        tags$label(
          class = "control-label",
          style = css(visibility = "hidden", width = "100%", marginBottom = "0.5rem"),
          "Parameters",
          `for` = ns("settings")
        ),
        dropMenu(
          placement = "bottom-end",
          actionButton(
            inputId = ns("settings"),
            label = phosphoricons::ph("gear", title = "parameters"),
            class = "btn-block"
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
            icon = phosphoricons::ph("text-aa")
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
        sprintf(i18n("You can import %s files"), paste(file_extensions, collapse = ", ")),
        dismissible = TRUE
      )
    ),
    if (isTRUE(preview_data)) {
      tagAppendAttributes(
        tableOutput(outputId = ns("table")),
        class = "datamods-table-container"
      )
    },
    uiOutput(
      outputId = ns("container_confirm_btn"),
      style = "margin-top: 20px;"
    )
  )
}


#' @inheritParams import_globalenv_server
#' @param read_fns Named list with custom function(s) to read data:
#'  * the name must be the extension of the files to which the function will be applied
#'  * the value must be a function that can have 5 arguments (you can ignore some of them, but you have to use the same names),
#'    passed by user through the interface:
#'    + `file`: path to the file
#'    + `sheet`: for Excel files, sheet to read
#'    + `skip`: number of row to skip
#'    + `dec`: decimal separator
#'    + `encoding`: file encoding
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom htmltools tags tagList
#' @importFrom shiny reactiveValues reactive observeEvent removeUI req renderTable
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom readxl excel_sheets
#' @importFrom rio import
#' @importFrom rlang exec fn_fmls_names is_named is_function
#' @importFrom tools file_ext
#' @importFrom utils head
#'
#' @rdname import-file
import_file_server <- function(id,
                               btn_show_data = TRUE,
                               show_data_in = c("popup", "modal"),
                               trigger_return = c("button", "change"),
                               return_class = c("data.frame", "data.table", "tbl_df", "raw"),
                               reset = reactive(NULL),
                               read_fns = list()) {

  if (length(read_fns) > 0) {
    if (!is_named(read_fns))
      stop("import_file_server: `read_fns` must be a named list.", call. = FALSE)
    if (!all(vapply(read_fns, is_function, logical(1))))
      stop("import_file_server: `read_fns` must be list of function(s).", call. = FALSE)
  }

  trigger_return <- match.arg(trigger_return)
  return_class <- match.arg(return_class)

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
      extension <- tools::file_ext(input$file$datapath)
      if (isTRUE(extension %in% names(read_fns))) {
        parameters <- list(
          file = input$file$datapath,
          sheet = input$sheet,
          skip = input$skip_rows,
          dec = input$dec,
          encoding = input$encoding
        )
        parameters <- parameters[which(names(parameters) %in% fn_fmls_names(read_fns[[extension]]))]
        imported <- try(rlang::exec(read_fns[[extension]], !!!parameters), silent = TRUE)
      } else {
        if (is_excel(input$file$datapath)) {
          req(input$sheet)
          parameters <- list(
            file = input$file$datapath,
            which = input$sheet,
            skip = input$skip_rows
          )
        } else if (is_sas(input$file$datapath)) {
          parameters <- list(
            file = input$file$datapath,
            skip = input$skip_rows,
            encoding = input$encoding
          )
        } else {
          parameters <- list(
            file = input$file$datapath,
            skip = input$skip_rows,
            dec = input$dec,
            encoding = input$encoding,
            na.strings = c("NA", "")
          )
        }
        imported <- try(rlang::exec(rio::import, !!!parameters), silent = TRUE)
      }

      if (inherits(imported, "try-error"))
        imported <- try(rlang::exec(rio::import, !!!parameters[1]), silent = TRUE)

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
      show_data(temporary_rv$data, title = i18n("Imported data"), type = show_data_in)
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

# utils -------------------------------------------------------------------

is_excel <- function(path) {
  isTRUE(tools::file_ext(path) %in% c("xls", "xlsx"))
}

is_sas <- function(path) {
  isTRUE(tools::file_ext(path) %in% c("sas7bdat"))
}

