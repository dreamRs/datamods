

#' @title Get all import functions in a Modal
#'
#' @description Let the user choose a method of his choice to import data
#'
#' @param id Module's id
#' @param from The import_ui & server to use, i.e. the method.
#'   There are 4 options to choose from. ("env", "file", "copypaste", "googlesheets")
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
#' @importFrom shiny NS tabsetPanel tabPanel icon fluidRow column
#' @importFrom htmltools tags HTML
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom DT DTOutput
#'
#' @example examples/modal.R
#'
import_ui <- function(id, from = c("env", "file", "copypaste", "googlesheets")) {
  ns <- NS(id)
  from <- match.arg(from, several.ok = TRUE)

  env <- if ("env" %in% from) {
    tabPanel(
      title = "env",
      tags$br(),
      import_globalenv_ui(id = ns("env"), title = NULL),
      icon = icon("code")
    )
  }

  file <- if ("file" %in% from) {
    tabPanel(
      title = "file",
      tags$br(),
      import_file_ui(id = ns("file"), title = NULL),
      icon = icon("file-import")
    )
  }

  copypaste <- if ("copypaste" %in% from) {
    tabPanel(
      title = "copypaste",
      tags$br(),
      import_copypaste_ui(id = ns("copypaste"), title = NULL),
      icon = icon("copy")
    )
  }

  googlesheets <- if ("googlesheets" %in% from) {
    tabPanel(
      title = "googlesheets",
      tags$br(),
      import_googlesheets_ui(id = ns("googlesheets"), title = NULL),
      icon = icon("cloud-download")
    )
  }

  #database <- if("database" %in% from) tabPanel("Database", import_database_ui(ns("database")))

  labsImport <- list(
    "env" = "Environment",
    "file" = "External file",
    "copypaste" = "Copy / Paste",
    "googlesheets" = "Googlesheets"
  )
  iconsImport <- list(
    "env" = icon("code"),
    "file" = icon("file-import"),
    "copypaste" = icon("copy"),
    "googlesheets" = icon("cloud-download")
  )


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
      type = "hidden"
    ))
    importTab <- do.call(
      what = tabsetPanel,
      args = tabsetPanelArgs
    )
    importTab <- fluidRow(
      column(
        width = 3,
        tags$br(),
        tags$style(
          HTML(sprintf("#%s>.btn-group-vertical {width: 100%%;}", ns("from"))),
          HTML(sprintf(".btn-group-vertical>.btn-group>.btn {text-align: left;}"))
        ),
        radioGroupButtons(
          inputId = ns("from"),
          label = "How to import data?",
          choiceValues = from,
          choiceNames = lapply(
            X = from,
            FUN = function(x) {
              tagList(iconsImport[[x]], labsImport[[x]])
            }
          ),
          direction = "vertical",
          width = "100%"
        )
      ),
      column(
        width = 9, importTab
      )
    )
  }

  tags$div(
    class = "datamods-imports",
    html_dependency_datamods(),
    tabsetPanel(
      type = "tabs",
      id = ns("tabs-mode"),
      tabPanel(
        title = "Import", importTab
      ),
      tabPanel(
        title = "View",
        tags$br(),
        DTOutput(outputId = ns("view"))
      ),
      tabPanel(
        title = "Update",
        tags$br(),
        update_variables_ui(id = ns("update"), title = NULL)
      ),
      tabPanel(
        title = "Validate",
        tags$br(),
        validation_ui(
          id = ns("validation"),
          display = "inline",
          max_height = "400px"
        )
      )
    ),
    tags$div(
      id = ns("confirm-button"),
      style = "margin-top: 20px;",
      button_import(list(ns = ns))
    ),
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = genId())
    ),
    tags$script(
      sprintf("$('#%s').addClass('nav-justified');", ns("tabs-mode")),
      sprintf("fadeTab({id: '%s'});", ns("tabs-mode")),
      sprintf("disableTab({id: '%s', value: '%s'});", ns("tabs-mode"), "View"),
      sprintf("disableTab({id: '%s', value: '%s'});", ns("tabs-mode"), "Update"),
      sprintf("disableTab({id: '%s', value: '%s'});", ns("tabs-mode"), "Validate")
    )
  )
}


#' @param validation_opts \code{list} of arguments passed to \code{\link{validation_server}}.
#' @param allowed_status Vector of statuses allowed to confirm dataset imported,
#'  if you want that all validation rules are successful before importing data use \code{allowed_status = "OK"}.
#' @param return_class Class of returned data: \code{data.frame}, \code{data.table} or \code{tbl_df} (tibble).
#'
#' @export
#' @rdname import-modal
#' @importFrom shiny moduleServer reactiveValues observeEvent
#'  reactive removeModal updateTabsetPanel hideTab observe
#' @importFrom DT tableHeader datatable renderDT
import_server <- function(id,
                          validation_opts = NULL,
                          allowed_status = c("OK", "Failed", "Error"),
                          return_class = c("data.frame", "data.table", "tbl_df")) {
  allowed_status <- match.arg(allowed_status, several.ok = TRUE)
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      data_rv <- reactiveValues(data = NULL)
      imported_rv <- reactiveValues(data = NULL)

      observeEvent(input$hidden, {
        if (length(validation_opts) < 1) {
          hideTab(inputId = "tabs-mode", target = "Validate")
        }
      })

      observeEvent(input$from, {
        updateTabsetPanel(
          session = session,
          inputId = "tabs-import",
          selected = input$from
        )
      })

      from_env <- import_globalenv_server(
        id = "env",
        trigger_return = "change",
        btn_show_data = FALSE
      )
      from_file <- import_file_server(
        id = "file",
        trigger_return = "change",
        btn_show_data = FALSE
      )
      from_copypaste <- import_copypaste_server(
        id = "copypaste",
        trigger_return = "change",
        btn_show_data = FALSE
      )
      from_googlesheets <- import_googlesheets_server(
        id = "googlesheets",
        trigger_return = "change",
        btn_show_data = FALSE
      )
      #from_database <- import_database_server("database")

      observeEvent(from_env$data(), {
        data_rv$data <- from_env$data()
        data_rv$name <- from_env$name()
      })
      observeEvent(from_file$data(), {
        data_rv$data <- from_file$data()
        data_rv$name <- from_file$name()
      })
      observeEvent(from_copypaste$data(), {
        data_rv$data <- from_copypaste$data()
        data_rv$name <- from_file$name()
      })
      observeEvent(from_googlesheets$data(), {
        data_rv$data <- from_googlesheets$data()
        data_rv$name <- from_file$name()
      })
      # observeEvent(from_database$data(), {
      #   data_rv$data <- from_database$data()
      # })

      observe({
        req(data_rv$data)
        if (is.data.frame(data_rv$data)) {
          if (length(validation_opts) < 1) {
            toggle_widget(inputId = "confirm", enable = TRUE)
          } else {
            status <- validation_results$status()
            req(status)
            if (status %in% allowed_status) {
              toggle_widget(inputId = "confirm", enable = TRUE)
            } else {
              toggle_widget(inputId = "confirm", enable = FALSE)
            }
          }
          enable_tab("tabs-mode", "View")
          enable_tab("tabs-mode", "Update")
          enable_tab("tabs-mode", "Validate")
        } else {
          toggle_widget(inputId = "confirm", enable = FALSE)
        }
      })

      output$view <- renderDT({
        req(data_rv$data)
        data <- data_rv$data
        classes <- get_classes(data)
        classes <- sprintf("<span style='font-style: italic; font-weight: normal; font-size: small;'>%s</span>", classes)
        container <- tags$table(
          tableHeader(paste(names(data), classes, sep = "<br>"), escape = FALSE)
        )
        datatable(
          data = data,
          rownames = FALSE,
          selection = "none",
          class = "display dt-responsive cell-border compact",
          style = "bootstrap",
          width = "100%",
          container = container,
          options = list(
            scrollX = TRUE,
            searching = FALSE,
            lengthChange = FALSE,
            pageLength = min(c(10, nrow(data_rv$data))),
            columnDefs = list(
              list(targets = "_all", className = "datamods-dt-nowrap")
            )
          )
        )
      })

      updated_data <- update_variables_server(
        id = "update",
        data = reactive(data_rv$data),
        height = "300px"
      )

      validation_results <- validation_server(
        id = "validation",
        data = reactive({
          data_rv$data
        }),
        n_row = validation_opts$n_row,
        n_col = validation_opts$n_col,
        n_row_label = validation_opts$n_row_label %||% "Valid number of rows",
        n_col_label = validation_opts$n_col_label %||% "Valid number of columns",
        btn_label = validation_opts$btn_label,
        rules = validation_opts$rules
      )

      observeEvent(validation_results$status(), {
        status <- validation_results$status()
        req(status)
        if (status %in% c("Error", "Failed")) {
          update_tab_label("tabs-mode", "Validate", tagList(
            tags$span(
              style = "color: firebrick;", icon("exclamation-circle")
            ), "Validate"
          ))
        } else {
          update_tab_label("tabs-mode", "Validate", "Validate")
        }
        if (status %in% allowed_status) {
          toggle_widget(inputId = "confirm", enable = TRUE)
        } else {
          toggle_widget(inputId = "confirm", enable = FALSE)
        }
      })

      observeEvent(updated_data(), {
        data_rv$data <- updated_data()
      })

      observeEvent(input$confirm, {
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
        `data-dismiss` = "modal",
        `aria-label` = "Close"
      ),
      title
    ),
    import_ui(id, from),
    size = size,
    footer = NULL
  ))
}


