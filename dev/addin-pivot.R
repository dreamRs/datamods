
library(shiny)
library(htmltools)
library(miniUI)
library(shinyWidgets)
library(tidyr)
library(data.table)
library(DT)
library(ggplot2)

headtail <- function(data, n = 5) {
  data <- rbindlist(
    list(
      head(data, n),
      tail(data, n)
    ), use.names = FALSE
  )
  data[] <- lapply(data, as.character)
  rbindlist(
    list(
      head(data, n),
      as.data.frame(do.call(cbind, as.list(rep_len("...", length.out = ncol(data))))),
      tail(data, n)
    ), use.names = FALSE
  )
}


ui <- fluidPage(
  tags$h3("Pivot data", style = "text-align: center;"),
  tabsetPanel(
    tabPanel(
      title = "To long format",
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "id_vars",
            label = "Variables to be kept in columns:",
            choices = NULL,
            multiple = TRUE,
            width = "100%"
          )
        ),
        column(
          width = 4,
          pickerInput(
            inputId = "measure_vars",
            label = "Variables to be transformed into rows:",
            choices = NULL,
            multiple = TRUE,
            width = "100%",
            options = pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE
            )
          )
        ),
        column(
          width = 4,
          dropMenu(
            placement = "bottom-start",
            maxWidth = "300px",
            actionButton(
              inputId = "parameters",
              label = "Other parameters",
              icon = icon("gears"),
              class = "btn-block",
              style = "margin-top: 25px;"
            ),
            textInput(
              inputId = "variable_name",
              label = "Variable name:",
              value = "variable",
              width = "200px"
            ),
            textInput(
              inputId = "value_name",
              label = "Value name:",
              value = "value",
              width = "200px"
            ),
            checkboxInput(
              inputId = "na_rm",
              label = "Remove NAs",
              value = FALSE
            ),
            actionButton(
              inputId = "apply_params",
              label = "Apply parameters",
              class = "btn-primary btn-block"
            )
          )
        )
      ),
      tagAppendAttributes(
        tableOutput("table_long"),
        style = "margin-top: 10px; overflow: auto; max-height: 500px; word-break: keep-all; white-space: nowrap;"
      )
    ),
    tabPanel(
      title = "To wide format"
    )
  )
)

server <- function(input, output, session) {

  data_long_rv <- reactiveValues(x = billboard)
  # data_long_rv <- reactiveValues(x = economics)

  observeEvent(data_long_rv$x, {
    updatePickerInput(
      session = session,
      inputId = "id_vars",
      choices = names(data_long_rv$x)
    )
    updatePickerInput(
      session = session,
      inputId = "measure_vars",
      choices = names(data_long_rv$x)
    )
  })

  observeEvent(list(input$id_vars, input$measure_vars), {
    if (is.null(input$id_vars) & is.null(input$measure_vars)) {
      data_long_rv$long <- data_long_rv$x
    } else {
      data_long_rv$long <- melt(
        data = as.data.table(data_long_rv$x),
        id.vars = input$id_vars,
        measure.vars = input$measure_vars
      )
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$apply_params, {
    hideDropMenu("parameters_dropmenu")
    if (is.null(input$id_vars) & is.null(input$measure_vars)) {
      data_long_rv$long <- data_long_rv$x
    } else {
      data_long_rv$long <- melt(
        data = as.data.table(data_long_rv$x),
        id.vars = input$id_vars,
        measure.vars = input$measure_vars,
        variable.name = input$variable_name,
        value.name = input$value_name,
        na.rm = input$na_rm
      )
    }
  })

  output$table_long <- renderTable({
    headtail(data_long_rv$long)
  }, striped = TRUE, bordered = TRUE, width = "100%")

}

# shinyApp(ui, server)
runGadget(app = shinyApp(ui, server), viewer = dialogViewer("Pivot", width = 1000))
