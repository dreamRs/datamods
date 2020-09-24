
library(shiny)
library(datamods)

ui <- fluidPage(
  fluidRow(
    column(
      width = 12,
      tags$b("Imported data:"),
      verbatimTextOutput(outputId = "result")
    )
  )
)

server <- function(input, output, session) {
  
  imported <- import_server("myid", "env")
  output$result <- renderPrint({
    req(imported$data())
    imported$data()
  })
}

if (interactive())
  shinyApp(ui, server)
