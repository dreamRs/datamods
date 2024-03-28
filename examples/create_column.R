
library(shiny)
library(datamods)
library(reactable)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5L, preset = "bootstrap"),
  tags$h2("Create new column"),
  fluidRow(
    column(
      width = 4,
      create_column_ui("col")
    ),
    column(
      width = 8,
      reactableOutput(outputId = "table")
    )
  )
)

server <- function(input, output, session) {

  data_r <- create_column_server(
    id = "col",
    data_r = reactive(MASS::Cars93[, c(1, 3, 4, 5, 6, 10)])
  )

  output$table <- renderReactable({
    data <- req(data_r())
    reactable(
      data = data,
      bordered = TRUE,
      compact = TRUE,
      striped = TRUE
    )
  })
}

if (interactive())
  shinyApp(ui, server)
