

#' @title Get all import functions in a Modal
#'
#' @description Let the user choose a method of his choice to import data
#'
#' @param id Module's id
#' @param from The import_ui & server to use, i.e. the method.
#'   There are 5 options to choose from. ("env", "file", "copypaste", "googlsheets" & "database")
#'
#' @return
#'  * UI: HTML tags that can be included in shiny's UI
#'  * Server: a \code{list} with two slots:
#'    + **data**: a \code{reactive} function returning the imported \code{data.frame}.
#'    + **name**: a \code{reactive} function returning the name of the imported data as \code{character} (if applicable).
#'
#' @export
#' @name import-modal
#'
#' @importFrom shiny NS tabsetPanel tabPanel icon
#'
#' @example examples/modal.R
#'
import_ui <- function(id, from = c("env", "file", "copypaste", "googlesheets", "database")) {
  ns <- NS(id)
  from <- match.arg(from, several.ok = TRUE)

  env <- if ("env" %in% from)
    tabPanel("Env", import_globalenv_ui(id = ns("env")), icon = icon("code"))

  file <- if ("file" %in% from)
    tabPanel("File", import_file_ui(id = ns("file")), icon = icon("file-import"))

  copypaste <- if ("copypaste" %in% from)
    tabPanel("Copy/Paste", import_copypaste_ui(id = ns("copypaste")), icon = icon("copy"))

  googlesheets <- if ("googlesheets" %in% from)
    tabPanel("Googlesheets", import_googlesheets_ui(id = ns("googlesheets")), icon = icon("cloud-download"))

  #database <- if("database" %in% from) tabPanel("Database", import_database_ui(ns("database")))

  if (identical(length(from), 1L)) {
    importTab <- switch(
      from,
      "env" = import_globalenv_ui(id = ns("env")),
      "file" = import_file_ui(id = ns("file")),
      "copypaste" = import_copypaste_ui(id = ns("copypaste")),
      "googlesheets" = import_googlesheets_ui(id = ns("googlesheets"))
    )
  } else {
    tabsetPanelArgs <- dropNulls(list(
      env, file, copypaste, googlesheets,
      id = ns("tabs-import"),
      type = "pills"
    ))
    importTab <- do.call(
      what = tabsetPanel,
      args = tabsetPanelArgs
    )
  }

  tags$div(
    class = "datamods-imports",
    html_dependency_datamods(),
    tabsetPanel(
      type = "hidden",
      id = ns("tabs-mode"),
      tabPanel(
        title = "import",
        importTab,
        tags$div(
          id = ns("validate-button"),
          style = "margin-top: 20px;",
          actionButton(
            inputId = ns("go_update"),
            label = "Select, rename and update data",
            icon = icon("gears"),
            width = "100%",
            disabled = "disabled",
            class = "btn-link"
          ),
          tags$div(
            class = "container-rule",
            tags$hr(class = "horizontal-rule"),
            tags$span("or", class = "label-rule")
          ),
          actionButton(
            inputId = ns("validate"),
            label = "Import data",
            icon = icon("arrow-circle-right"),
            width = "100%",
            disabled = "disabled",
            class = "btn-primary"
          )
        )
      ),
      tabPanel(
        title = "update",
        update_variables_ui(id = ns("update"))
      )
    ),
    tags$script(
      sprintf("$('#%s').addClass('nav-justified');", ns("tabs-import"))
    )
  )
}


#' @param return_class Class of returned data: \code{data.frame}, \code{data.table} or \code{tbl_df} (tibble).
#'
#' @export
#' @rdname import-modal
#' @importFrom shiny moduleServer reactiveValues observeEvent reactive removeModal updateTabsetPanel
import_server <- function(id,
                          return_class = c("data.frame", "data.table", "tbl_df")) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      data_rv <- reactiveValues(data = NULL)
      imported_rv <- reactiveValues(data = NULL)

      from_env <- import_globalenv_server(
        id = "env",
        trigger_return = "change"
      )
      from_file <- import_file_server(
        id = "file",
        trigger_return = "change"
      )
      from_copypaste <- import_copypaste_server(
        id = "copypaste",
        trigger_return = "change"
      )
      from_googlesheets <- import_googlesheets_server(
        id = "googlesheets",
        trigger_return = "change"
      )
      #from_database <- import_database_server("database")

      observeEvent(from_env$data(), {
        data_rv$data <- from_env$data()
        data_rv$name <- from_env$name()
      })
      observeEvent(from_file$data(), {
        data_rv$data <- from_file$data()
        data_rv$name <- NULL
      })
      observeEvent(from_copypaste$data(), {
        data_rv$data <- from_copypaste$data()
        data_rv$name <- NULL
      })
      observeEvent(from_googlesheets$data(), {
        data_rv$data <- from_googlesheets$data()
        data_rv$name <- NULL
      })
      # observeEvent(from_database$data(), {
      #   data_rv$data <- from_database$data()
      # })

      observeEvent(data_rv$data, {
        if (is.data.frame(data_rv$data)) {
          toggle_widget(inputId = "validate", enable = TRUE)
          toggle_widget(inputId = "go_update", enable = TRUE)
        } else {
          toggle_widget(inputId = "validate", enable = FALSE)
          toggle_widget(inputId = "go_update", enable = FALSE)
        }
      })


      observeEvent(input$go_update, {
        updateTabsetPanel(
          session = session,
          inputId = "tabs-mode",
          selected = "update"
        )
      })

      updated_data <- update_variables_server(
        id = "update",
        data = reactive(data_rv$data)
      )

      observeEvent(updated_data(), {
        removeModal()
        imported_rv$data <- updated_data()
        imported_rv$name <- data_rv$name %||% "imported_data"
      })

      observeEvent(input$validate, {
        removeModal()
        imported_rv$data <- data_rv$data
        imported_rv$name <- data_rv$name %||% "imported_data"
      })

      return(list(
        data = reactive(as_out(imported_rv$data, return_class)),
        name = reactive(imported_rv$name)
      ))
    }
  )
}


#' @param title Modal window title.
#' @param size Modal window size, default to \code{"l"} (large).
#'
#' @export
#' @rdname import-modal
#' @importFrom shiny modalDialog showModal
import_modal <- function(id, from, title = "Import data", size = "l") {
  showModal(modalDialog(
    title = tagList(
      tags$button(
        icon("close"),
        class = "btn btn-default pull-right",
        style = "border: 0 none;",
        `data-dismiss` = "modal"
      ),
      title
    ),
    import_ui(id, from),
    size = size,
    footer = NULL
  ))
}


