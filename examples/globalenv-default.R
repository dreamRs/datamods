
library(shiny)
library(datamods)

# Create some data.frames

my_df <- data.frame(
  variable1 = sample(letters, 20, TRUE),
  variable2 = sample(1:100, 20, TRUE)
)

results_analysis <- data.frame(
  id = sample(letters, 20, TRUE),
  measure = sample(1:100, 20, TRUE),
  response = sample(1:100, 20, TRUE)
)


# Application

ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      import_globalenv_ui("myid")
    ),
    column(
      width = 8,
      tags$b("Imported data:"),
      verbatimTextOutput(outputId = "status"),
      verbatimTextOutput(outputId = "name"),
      verbatimTextOutput(outputId = "data")
    )
  )
)

server <- function(input, output, session) {

  imported <- import_globalenv_server("myid")

  output$status <- renderPrint({
    imported$status()
  })
  output$name <- renderPrint({
    imported$name()
  })
  output$data <- renderPrint({
    imported$data()
  })

}

if (interactive())
  shinyApp(ui, server)
