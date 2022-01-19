
library(shiny)
library(datamods)

ui <- fluidPage(
  tags$h3("Import data from API"),
  fluidRow(
    column(
      width = 4,
      import_api_ui("myid")
    ),
    column(
      width = 8,
      tags$b("Import status:"),
      verbatimTextOutput(outputId = "status"),
      tags$b("Name:"),
      verbatimTextOutput(outputId = "name"),
      tags$b("Data:"),
      verbatimTextOutput(outputId = "data")
    )
  )
)

server <- function(input, output, session) {
  
  imported <- import_api_server("myid")
  
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
