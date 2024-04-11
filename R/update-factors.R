
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
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom toastui datagridOutput2
#' @importFrom htmltools tags
#'
#' @name update-factors
#'
#' @example examples/update_factors.R
update_factors_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        virtualSelectInput(
          inputId = ns("variable"),
          label = "Factor variable to reorder:",
          choices = NULL,
          width = "100%"
        )
      ),
      column(
        width = 4,
        actionButton(
          inputId = ns("sort_levels"),
          label = tagList(
            ph("sort-ascending"),
            "Sort Levels"
          )
        )
      ),
      column(
        width = 4,
        actionButton(
          inputId = ns("sort_occurrences"),
          label = tagList(
            ph("sort-ascending"),
            "Sort Number of Occurrences"
          )
        )
      )
    ),
    datagridOutput2(ns("grid")),
    actionButton(
      inputId = ns("create"),
      label = tagList(ph("arrow-clockwise"), "Update factor variable"),
      class = "btn-outline-primary float-end"
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
#' @importFrom toastui renderDatagrid2 datagrid grid_columns grid_colorbar
#' @importFrom forcats fct_relevel fct_inorder
#'
#' @rdname update-factors
update_factors_server <- function(id, data_r = reactive(NULL)) {
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
      }), data_r())
      
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
            "Sort Number of Occurrences"
          )
        } else {
          decreasing <- TRUE
          label <- tagList(
            ph("sort-ascending"),
            "Sort Number of Occurrences"
          )
        }
        updateActionButton(inputId = "sort_occurrences", label = as.character(label))
        rv$data_grid <- rv$data_grid[order(rv$data_grid[[2]], decreasing = decreasing), ]
      })
      
      data_updated_r <- reactive({
        data <- req(data_r())
        variable <- req(input$variable)
        grid <- req(input$grid_data)
        # grid <- req(rv$data_grid)
        data[[paste0(variable, "_updated")]] <- factor(
          as.character(data[[variable]]),
          levels(as.factor(grid[["Var1"]]))
        )
        data
      })
      
      output$grid <- renderDatagrid2({
        datagrid(
          data =  rv$data_grid, 
          draggable = TRUE,
          sortable = FALSE,
          data_as_input = TRUE
        ) %>%
          grid_columns(
            columns = c("Var1", "Freq"),
            header = c("Levels", "Number of occurences")
          ) %>%
          grid_colorbar(
            column = "Freq",
            label_outside = TRUE,
            label_width = "30px",
            background = "#D8DEE9",
            from = c(0, nrow((data_r())))
          )
      })
      
      data_returned_r <- observeEvent(input$create, {
        rv$data <- data_updated_r()
      })
      return(reactive(rv$data))
    }
  )
}
