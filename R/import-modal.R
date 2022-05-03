
#' @title Import from all sources
#'
#' @description Wrap all import modules into one, can be displayed inline or in a modal window..
#'
#' @param id Module's id
#' @param from The import_ui & server to use, i.e. the method.
#'   There are 5 options to choose from. ("env", "file", "copypaste", "googlesheets", "url")
#'
#' @template module-import
#'
#' @export
#' @name import-modal
#'
#' @importFrom shiny NS tabsetPanel tabPanel tabPanelBody icon fluidRow column
#' @importFrom htmltools tags HTML
#' @importFrom shinyWidgets radioGroupButtons
#'
#' @example examples/modal.R
#'
import_ui <- function(id, from = c("env", "file", "copypaste", "googlesheets", "url")) {
  ns <- NS(id)
  from <- match.arg(from, several.ok = TRUE)

  env <- if ("env" %in% from) {
    tabPanelBody(
      value = "env",
      tags$br(),
      import_globalenv_ui(id = ns("env"), title = NULL)
    )
  }

  file <- if ("file" %in% from) {
    tabPanelBody(
      value = "file",
      tags$br(),
      import_file_ui(id = ns("file"), title = NULL)
    )
  }

  copypaste <- if ("copypaste" %in% from) {
    tabPanelBody(
      value = "copypaste",
      tags$br(),
      import_copypaste_ui(id = ns("copypaste"), title = NULL)
    )
  }

  googlesheets <- if ("googlesheets" %in% from) {
    tabPanelBody(
      value = "googlesheets",
      tags$br(),
      import_googlesheets_ui(id = ns("googlesheets"), title = NULL)
    )
  }

  url <- if ("url" %in% from) {
    tabPanelBody(
      value = "url",
      tags$br(),
      import_url_ui(id = ns("url"), title = NULL)
    )
  }

  #database <- if("database" %in% from) tabPanel("Database", import_database_ui(ns("database")))

  labsImport <- list(
    "env" = i18n("Environment"),
    "file" = i18n("External file"),
    "copypaste" = i18n("Copy / Paste"),
    "googlesheets" = i18n("Googlesheets"),
    "url" = i18n("URL")
  )
  iconsImport <- list(
    "env" = phosphoricons::ph("code", title = labsImport$env),
    "file" = phosphoricons::ph("file-arrow-down", title = labsImport$file),
    "copypaste" = phosphoricons::ph("clipboard-text", title = labsImport$copypaste),
    "googlesheets" = phosphoricons::ph("cloud-arrow-down", title = labsImport$googlesheets),
    "url" = phosphoricons::ph("link", title = labsImport$url)
  )


  if (identical(length(from), 1L)) {
    importTab <- switch(
      from,
      "env" = import_globalenv_ui(id = ns("env")),
      "file" = import_file_ui(id = ns("file")),
      "copypaste" = import_copypaste_ui(id = ns("copypaste")),
      "googlesheets" = import_googlesheets_ui(id = ns("googlesheets")),
      "url" = import_url_ui(id = ns("url")),
    )
  } else {
    tabsetPanelArgs <- dropNulls(list(
      env, file, copypaste, googlesheets, url,
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
          label = i18n("How to import data?"),
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
        title = tagList(
          phosphoricons::ph("download-simple", title = i18n("Import")),
          i18n("Import")
        ),
        value = "import",
        importTab
      ),
      tabPanel(
        title = tagList(
          phosphoricons::ph("table", title = i18n("View")),
          i18n("View")
        ),
        value = "view",
        tags$br(),
        reactable::reactableOutput(outputId = ns("view"))
      ),
      tabPanel(
        title = tagList(
          phosphoricons::ph("gear-six", title = i18n("Update")),
          i18n("Update")
        ),
        value = "update",
        tags$br(),
        update_variables_ui(id = ns("update"), title = NULL)
      ),
      tabPanel(
        title = tagList(
          phosphoricons::ph("shield-check", title = i18n("Validate")),
          i18n("Validate")
        ),
        value = "validate",
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
      sprintf("disableTab({id: '%s', value: '%s'});", ns("tabs-mode"), "view"),
      sprintf("disableTab({id: '%s', value: '%s'});", ns("tabs-mode"), "update"),
      sprintf("disableTab({id: '%s', value: '%s'});", ns("tabs-mode"), "validate")
    )
  )
}


#' @param validation_opts `list` of arguments passed to [validation_server().
#' @param allowed_status Vector of statuses allowed to confirm dataset imported,
#'  if you want that all validation rules are successful before importing data use `allowed_status = "OK"`.
#' @param return_class Class of returned data: `data.frame`, `data.table` or `tbl_df` (tibble).
#'
#' @export
#' @rdname import-modal
#' @importFrom shiny moduleServer reactiveValues observeEvent
#'  reactive removeModal updateTabsetPanel hideTab observe
#' @importFrom rlang %||%
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
        data_rv$data <- NULL
        data_rv$name <- NULL
        if (length(validation_opts) < 1) {
          hideTab(inputId = "tabs-mode", target = "validate")
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
        btn_show_data = FALSE,
        reset = reactive(input$hidden)
      )
      from_file <- import_file_server(
        id = "file",
        trigger_return = "change",
        btn_show_data = FALSE,
        reset = reactive(input$hidden)
      )
      from_copypaste <- import_copypaste_server(
        id = "copypaste",
        trigger_return = "change",
        btn_show_data = FALSE,
        reset = reactive(input$hidden)
      )
      from_googlesheets <- import_googlesheets_server(
        id = "googlesheets",
        trigger_return = "change",
        btn_show_data = FALSE,
        reset = reactive(input$hidden)
      )
      from_url <- import_url_server(
        id = "url",
        trigger_return = "change",
        btn_show_data = FALSE,
        reset = reactive(input$hidden)
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
        data_rv$name <- from_copypaste$name()
      })
      observeEvent(from_googlesheets$data(), {
        data_rv$data <- from_googlesheets$data()
        data_rv$name <- from_googlesheets$name()
      })
      observeEvent(from_url$data(), {
        data_rv$data <- from_url$data()
        data_rv$name <- from_url$name()
      })
      # observeEvent(from_database$data(), {
      #   data_rv$data <- from_database$data()
      # })

      observeEvent(data_rv$data, {
        req(data_rv$data)
        if (is.data.frame(data_rv$data)) {
          if (length(validation_opts) < 1) {
            toggle_widget(inputId = "confirm", enable = TRUE)
          } else {
            status <- validation_results$status()
            if (isTRUE(status %in% allowed_status)) {
              toggle_widget(inputId = "confirm", enable = TRUE)
            } else {
              toggle_widget(inputId = "confirm", enable = FALSE)
            }
          }
          enable_tab("tabs-mode", "view")
          enable_tab("tabs-mode", "update")
          enable_tab("tabs-mode", "validate")
        } else {
          toggle_widget(inputId = "confirm", enable = FALSE)
        }
      })

      output$view <- reactable::renderReactable({
        data <- req(data_rv$data)
        reactable::reactable(
          data,
          defaultColDef = reactable::colDef(
            header = function(value) {
              classes <- tags$div(
                style = "font-style: italic; font-weight: normal; font-size: small;",
                get_classes(data[, value, drop = FALSE])
              )
              tags$div(title = value, value, classes)
            }
          ),
          columns = list(),
          bordered = TRUE,
          compact = TRUE,
          striped = TRUE
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
          update_tab_label("tabs-mode", "validate", tagList(
            phosphoricons::ph("warning-circle", weight = "fill", fill = "firebrick"), i18n("Validate")
          ))
        } else {
          update_tab_label("tabs-mode", "validate", i18n("Validate"))
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
#' @importFrom htmltools tags css
import_modal <- function(id, from, title = "Import data", size = "l") {
  showModal(modalDialog(
    title = tagList(
      tags$button(
        phosphoricons::ph("x", title = i18n("Close"), height = "2em"),
        class = "btn btn-link",
        style = css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
        `data-dismiss` = "modal",
        `data-bs-dismiss` = "modal",
        `aria-label` = i18n("Close")
      ),
      title
    ),
    import_ui(id, from),
    size = size,
    footer = NULL
  ))
}


