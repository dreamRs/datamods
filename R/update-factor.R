
#' @title Module to Reorder the Levels of a Factor Variable
#'
#' @description
#' This module contain an interface to reorder the levels of a factor variable.
#'
#'
#' @param id Module ID.
#'
#' @return A [shiny::reactive()] function returning the data.
#' @export
#'
#' @importFrom shiny NS fluidRow tagList column actionButton
#' @importFrom shinyWidgets virtualSelectInput prettyCheckbox
#' @importFrom toastui datagridOutput
#' @importFrom htmltools tags
#'
#' @name update-factor
#'
#' @example examples/update_factor.R
update_factor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      ".tui-grid-row-header-draggable span {width: 3px !important; height: 3px !important;}"
    ),
    fluidRow(
      column(
        width = 6,
        virtualSelectInput(
          inputId = ns("variable"),
          label = i18n("Factor variable to reorder:"),
          choices = NULL,
          width = "100%",
          zIndex = 50
        )
      ),
      column(
        width = 3,
        class = "d-flex align-items-end",
        actionButton(
          inputId = ns("sort_levels"),
          label = tagList(
            ph("sort-ascending"),
            i18n("Sort by levels")
          ),
          class = "btn-outline-primary mb-3",
          width = "100%"
        )
      ),
      column(
        width = 3,
        class = "d-flex align-items-end",
        actionButton(
          inputId = ns("sort_occurrences"),
          label = tagList(
            ph("sort-ascending"),
            i18n("Sort by count")
          ),
          class = "btn-outline-primary mb-3",
          width = "100%"
        )
      )
    ),
    datagridOutput(ns("grid")),
    tags$div(
      class = "float-end",
      prettyCheckbox(
        inputId = ns("new_var"),
        label = i18n("Create a new variable (otherwise replaces the one selected)"),
        value = FALSE,
        status = "primary",
        outline = TRUE,
        inline = TRUE
      ),
      actionButton(
        inputId = ns("create"),
        label = tagList(ph("arrow-clockwise"), i18n("Update factor variable")),
        class = "btn-outline-primary"
      )
    ),
    tags$div(class = "clearfix")
  )
}


#' @param data_r A [shiny::reactive()] function returning a `data.frame`.
#'
#' @export
#'
#' @importFrom shiny moduleServer observeEvent reactive reactiveValues req bindEvent isTruthy updateActionButton
#' @importFrom shinyWidgets updateVirtualSelect
#' @importFrom toastui renderDatagrid datagrid grid_columns grid_colorbar
#'
#' @rdname update-factor
update_factor_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(data = NULL, data_grid = NULL)

      bindEvent(observe({
        data <- data_r()
        rv$data <- data
        vars_factor <- vapply(data, is.factor, logical(1))
        vars_factor <- names(vars_factor)[vars_factor]
        updateVirtualSelect(
          inputId = "variable",
          choices = vars_factor,
          selected = if (isTruthy(input$variable)) input$variable else vars_factor[1]
        )
      }), data_r(), input$hidden)

      observeEvent(input$variable, {
        data <- req(data_r())
        variable <- req(input$variable)
        grid <- as.data.frame(table(data[[variable]]))
        rv$data_grid <- grid
      })

      observeEvent(input$sort_levels, {
        if (input$sort_levels %% 2 == 1) {
          decreasing <- FALSE
          label <- tagList(
            ph("sort-descending"),
            "Sort Levels"
          )
        } else {
          decreasing <- TRUE
          label <- tagList(
            ph("sort-ascending"),
            "Sort Levels"
          )
        }
        updateActionButton(inputId = "sort_levels", label = as.character(label))
        rv$data_grid <- rv$data_grid[order(rv$data_grid[[1]], decreasing = decreasing), ]
      })

      observeEvent(input$sort_occurrences, {
        if (input$sort_occurrences %% 2 == 1) {
          decreasing <- FALSE
          label <- tagList(
            ph("sort-descending"),
            i18n("Sort count")
          )
        } else {
          decreasing <- TRUE
          label <- tagList(
            ph("sort-ascending"),
            i18n("Sort count")
          )
        }
        updateActionButton(inputId = "sort_occurrences", label = as.character(label))
        rv$data_grid <- rv$data_grid[order(rv$data_grid[[2]], decreasing = decreasing), ]
      })


      output$grid <- renderDatagrid({
        req(rv$data_grid)
        gridTheme <- getOption("datagrid.theme")
        if (length(gridTheme) < 1) {
          apply_grid_theme()
        }
        on.exit(toastui::reset_grid_theme())
        grid <- datagrid(
          data = rv$data_grid,
          draggable = TRUE,
          sortable = FALSE,
          data_as_input = TRUE
        )
        grid <- grid_columns(
          grid,
          columns = c("Var1", "Freq"),
          header = c(i18n("Levels"), i18n("Count"))
        )
        grid <- grid_colorbar(
          grid,
          column = "Freq",
          label_outside = TRUE,
          label_width = "30px",
          background = "#D8DEE9",
          bar_bg = get_primary_color(),
          from = c(0, max(rv$data_grid$Freq) + 1)
        )
        grid
      })

      data_updated_r <- reactive({
        data <- req(data_r())
        variable <- req(input$variable)
        grid <- req(input$grid_data)
        name_var <- if (isTRUE(input$new_var)) {
          paste0(variable, "_updated")
        } else {
          variable
        }
        data[[name_var]] <- factor(
          as.character(data[[variable]]),
          levels = grid[["Var1"]]
        )
        data
      })

      data_returned_r <- observeEvent(input$create, {
        rv$data <- data_updated_r()
      })
      return(reactive(rv$data))
    }
  )
}



#' @inheritParams shiny::modalDialog
#' @export
#'
#' @importFrom shiny showModal modalDialog textInput
#' @importFrom htmltools tagList
#'
#' @rdname update-factor
modal_update_factor <- function(id,
                                title = i18n("Update levels of a factor"),
                                easyClose = TRUE,
                                size = "l",
                                footer = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(title, button_close_modal()),
    update_factor_ui(id),
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = genId())
    ),
    easyClose = easyClose,
    size = size,
    footer = footer
  ))
}


#' @inheritParams shinyWidgets::WinBox
#' @export
#'
#' @importFrom shinyWidgets WinBox wbOptions wbControls
#' @importFrom htmltools tagList
#' @rdname create-column
winbox_update_factor <- function(id,
                                 title = i18n("Update levels of a factor"),
                                 options = shinyWidgets::wbOptions(),
                                 controls = shinyWidgets::wbControls()) {
  ns <- NS(id)
  WinBox(
    title = title,
    ui = tagList(
      update_factor_ui(id),
      tags$div(
        style = "display: none;",
        textInput(inputId = ns("hidden"), label = NULL, value = genId())
      )
    ),
    options = modifyList(
      shinyWidgets::wbOptions(height = "615px", modal = TRUE),
      options
    ),
    controls = controls,
    auto_height = FALSE
  )
}


