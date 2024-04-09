
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
#' @importFrom shiny NS fluidRow tagList column actionButton verbatimTextOutput
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
        width = 3,
        virtualSelectInput(
          inputId = ns("variable"),
          label = "Factor variable to reorder:",
          choices = NULL,
          width = "100%"
        )
      ),
      column(
        width = 9,
        # radioGroupButtons(
        #   inputId = ns("sort"),
        #   label = "Sort:",
        #   choices = c(
        #     "Increasing levels" = "increasing_levels", 
        #     "Decreasing levels" = "decreasing_levels",
        #     "Increasing occurrences" = "increasing_occurrences", 
        #     "Decreasing occurrences" = "decreasing_occurrences"
        #   ),
        #   selected = "increasing_levels",
        #   width = "100%"
        # ),
        datagridOutput2(ns("grid"))
      )
    ),
    actionButton(
      inputId = ns("create"),
      label = tagList(ph("arrow-clockwise"), "Update factor variable"),
      class = "btn-outline-primary float-end"
    ),
    tags$div(class = "clearfix"),
    tags$b("Data:"),
    verbatimTextOutput(ns("data")),
  )
}


#' @param data_r A [shiny::reactive()] function returning a `data.frame`.
#'
#' @export
#'
#' @importFrom shiny moduleServer observe observeEvent reactive reactiveValues req bindEvent isTruthy renderPrint
#' @importFrom shinyWidgets updateVirtualSelect
#' @importFrom toastui renderDatagrid2 datagrid grid_columns grid_colorbar
#' @importFrom forcats fct_relevel fct_inorder
#'
#' @rdname update-factors
update_factors_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      
      rv <- reactiveValues(data = NULL) # data_grid = NULL

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
      
      data_updated_r <- reactive({
        data <- req(data_r())
        variable <- req(input$variable)
        data_grid <- req(input$grid_data)
        data[[paste0(variable, "_updated")]] <- fct_relevel(
          data[[variable]],
          levels(fct_inorder(as.factor(data_grid[["Var1"]])))
        )
        data
      })
      
      output$grid <- renderDatagrid2({
        data_r <- data_r()
        variable <- req(input$variable)
        data_grid <- as.data.frame(table(data_r[[variable]]))

        # if (input$sort == "increasing_levels") {
        #   rv$data_grid <- data_grid %>%
        #     arrange(Var1)
        # }
        # if (input$sort == "decreasing_levels") {
        #   rv$data_grid <- data_grid %>%
        #     arrange(desc(Var1))
        # } else if (input$sort == "increasing_occurrences") {
        #   rv$data_grid <- data_grid %>%
        #     arrange(Freq)
        # } else if (input$sort == "decreasing_occurrences"){
        #   rv$data_grid <- data_grid %>%
        #     arrange(desc(Freq))
        # } else {
        #   NULL
        # }
        
        datagrid(
          data =  data_grid, #rv$data_grid
          draggable = TRUE,
          # sortable = FALSE,
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
      
      output$data <- renderPrint({
        input$grid_data
      })
      
      
      data_returned_r <- observeEvent(input$create, {
        rv$data <- data_updated_r()
      })
      return(reactive(rv$data))
    }
  )
}
