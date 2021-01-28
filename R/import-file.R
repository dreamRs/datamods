
#' @title Import data from a file
#'
#' @description Let user upload a file and import data
#'
#' @param id Module's ID.
#' @param title Module's title, if \code{TRUE} use the default title,
#'  use \code{NULL} for no title or a \code{shiny.tag} for a custom one.
#'
#' @return
#'  * UI: HTML tags that can be included in shiny's UI
#'  * Server: a \code{list} with one slot:
#'    + **data**: a \code{reactive} function returning the imported \code{data.frame}.
#'
#' @export
#' @name import-file
#'
#'
#' @importFrom shiny NS fileInput
#' @importFrom htmltools tags
#' @importFrom shinyWidgets pickerInput numericInputIcon
#'
#' @example examples/from-file.R
import_file_ui <- function(id, title = TRUE) {

  ns <- NS(id)

  if (isTRUE(title)) {
    title <- tags$h4("Import a file", class = "datamods-title")
  }

  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    title,
    fileInput(
      inputId = ns("file"),
      label = "Upload a file:",
      accept = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav"),
      width = "100%"
    ),
    tags$div(
      class = "hidden",
      id = ns("sheet-container"),
      pickerInput(
        inputId = ns("sheet"),
        label = "Select sheet to import:",
        choices = NULL,
        width = "100%"
      )
    ),
    numericInputIcon(
      inputId = ns("skip_rows"),
      label = "Number of rows to skip before reading data:",
      value = 0,
      min = 0,
      width = "100%",
      icon = list("n =")
    ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b("No file selected:"), "You can import .rds, .txt, .csv, .xls, .xlsx, .sas7bdat, .sav, ...",
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
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom htmltools tags tagList
#' @importFrom shiny reactiveValues reactive observeEvent removeUI req
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom readxl excel_sheets
#' @importFrom rio import
#' @importFrom tools file_ext
#'
#' @rdname import-file
import_file_server <- function(id,
                               btn_show_data = TRUE,
                               trigger_return = c("button", "change"),
                               return_class = c("data.frame", "data.table", "tbl_df")) {

  trigger_return <- match.arg(trigger_return)

  module <- function(input, output, session) {

    ns <- session$ns
    imported_rv <- reactiveValues(data = NULL)
    temporary_rv <- reactiveValues(data = NULL)

    output$container_valid_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        actionButton(
          inputId = ns("validate"),
          label = "Import data",
          icon = icon("arrow-circle-right"),
          width = "100%",
          disabled = "disabled",
          class = "btn-primary"
        )
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
      input$skip_rows
    ), {
      req(input$file)
      req(input$skip_rows)
      if (is_excel(input$file$datapath)) {
        req(input$sheet)
        imported <- try(rio::import(file = input$file$datapath, which = input$sheet, skip = input$skip_rows), silent = TRUE)
      } else {
        imported <- try(rio::import(file = input$file$datapath, skip = input$skip_rows), silent = TRUE)
      }

      if (inherits(imported, "try-error") || NROW(imported) < 1) {

        toggle_widget(inputId = "validate", enable = FALSE)

        insert_alert(
          selector = ns("import"),
          status = "danger",
          tags$b(icon("exclamation-triangle"), "Ooops"), "Something went wrong..."
        )

      } else {

        toggle_widget(inputId = "validate", enable = TRUE)

        insert_alert(
          selector = ns("import"),
          status = "success",
          make_success_alert(
            imported,
            trigger_return = trigger_return,
            btn_show_data = btn_show_data
          )
        )

        temporary_rv$data <- imported
      }
    }, ignoreInit = TRUE)

    observeEvent(input$see_data, {
      show_data(temporary_rv$data)
    })

    observeEvent(input$validate, {
      imported_rv$data <- temporary_rv$data
    })

    if (identical(trigger_return, "button")) {
      return(list(
        data = reactive(as_out(imported_rv$data, return_class))
      ))
    } else {
      return(list(
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

