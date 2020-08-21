
library(shiny)
library(datamods)

ui <- fluidPage(
  tags$h3("Import data with googlesheets"),
  fluidRow(
    column(
      width = 4,
      import_googlesheets_ui("myid")
    ),
    column(
      width = 8,
      tags$b("Imported data:"),
      verbatimTextOutput(outputId = "result")
    )
  )
)

server <- function(input, output, session) {
  
  imported <- import_googlesheets_server("myid")
  
  output$result <- renderPrint({
    imported$data()
  })
  
}

if (interactive())
  shinyApp(ui, server)
