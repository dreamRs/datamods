
#' @title Import data from Googlesheet
#'
#' @description Let user paste link to a Google sheet then import the data.
#'
#' @inheritParams import-globalenv
#'
#' @template module-import
#'
#' @export
#' @name import-googlesheets
#'
#' @importFrom shiny NS actionLink
#' @importFrom shinyWidgets textInputIcon
#' @importFrom htmltools tags tagList
#'
#' @example examples/from-googlesheets.R
import_googlesheets_ui <- function(id, title = TRUE) {

  ns <- NS(id)

  if (isTRUE(title)) {
    title <- tags$h4(
      i18n("Import Google Spreadsheet"),
      class = "datamods-title"
    )
  }

  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    title,
    tags$div(
      class = "pull-right float-right",
      help_popup(tagList(
        i18n("You can either use:"),
        tags$ul(
          tags$li(
            i18n("A shareable link, in that case first sheet will be read")
          ),
          tags$li(
            i18n("The URL that appear in your browser, in that case the current sheet will be read")
          )
        )
      ))
    ),
    textInputIcon(
      inputId = ns("link"),
      label = i18n("Enter a shareable link to a GoogleSheet:"),
      icon = phosphoricons::ph("link"),
      width = "100%"
    ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b(i18n("Nothing pasted yet!")),
        i18n("Please paste a valid GoogleSheet link in the dialog box above."),
        dismissible = TRUE
      )
    ),
    uiOutput(
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
#' @rdname import-googlesheets
import_googlesheets_server <- function(id,
                                       btn_show_data = TRUE,
                                       show_data_in = c("popup", "modal"),
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
      imported <- try(read_gsheet(input$link), silent = TRUE)
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
      show_data(temporary_rv$data, title = i18n("Imported data"), type = show_data_in)
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

get_id <- function(x) {
  if (grepl("/d/", x)) {
    x <- strsplit(x = x, split = "/")
    x <- unlist(x)
    x[which(x == "d") + 1]
  } else if (grepl("id=", x)) {
    x <- regmatches(x, gregexpr("id=[[:alnum:]_-]+", x))
    gsub("^id=", "", x[[1]])
  } else {
    stop("Failed to retrieve Googlesheet ID")
  }
}

#' @importFrom data.table fread .SD
#' @importFrom utils type.convert
read_gsheet <- function(url, dec = NULL) {
  url_ <- sprintf(
    "https://docs.google.com/spreadsheets/export?id=%s&format=csv",
    get_id(url)
  )
  if (grepl("gid=", url)) {
    gid <- regmatches(url, gregexpr("gid=[0-9]+", url))
    url_ <- paste0(url_, "&", gid[[1]])
  }
  dt <- fread(input = url_)
  if (!is.null(dec)) {
    dt <- dt[, lapply(.SD, type.convert, dec = dec)]
  }
  return(dt)
}

