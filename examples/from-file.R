

library(shiny)
library(datamods)

ui <- fluidPage(
  tags$h3("Import data from a file"),
  fluidRow(
    column(
      width = 4,
      import_file_ui("myid")
    ),
    column(
      width = 8,
      tags$b("Imported data:"),
      verbatimTextOutput(outputId = "result")
    )
  )
)

server <- function(input, output, session) {
  
  imported <- import_file_server("myid")
  
  output$result <- renderPrint({
    imported$data()
  })
  
}

if (interactive())
  shinyApp(ui, server)
