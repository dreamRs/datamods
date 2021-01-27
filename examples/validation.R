library(datamods)
library(shiny)
library(validate)

ui <- fluidPage(
  tags$h2("Validation"),
  fluidRow(
    column(
      width = 4,
      radioButtons(
        inputId = "dataset",
        label = "Choose dataset:",
        choices = c("cars", "mtcars")
      ),
      validation_ui("validation")
    ),
    column(
      width = 8,
      tags$b("Status:"),
      verbatimTextOutput("status"),
      tags$b("Details:"),
      verbatimTextOutput("details")
    )
  )
)

server <- function(input, output, session) {

  dataset <- reactive({
    get(input$dataset)
  })

  results <- validation_server(
    id = "validation",
    data = dataset,
    n_row = ~ . > 20, # more than 20 rows
    n_col = ~ . >= 3, # at least 3 columns
    rules = validator(
      "disp exist" = !is.null(disp),
      is.numeric(disp),
      "mpg positive" = mpg > 0 # check that variable mpg is positive
    )
  )

  output$status <- renderPrint(results$status())
  output$details <- renderPrint(results$details())

}

if (interactive())
  shinyApp(ui, server)
