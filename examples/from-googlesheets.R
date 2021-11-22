
library(shiny)
library(datamods)

ui <- fluidPage(
  tags$h3("Import data from Googlesheets"),
  fluidRow(
    column(
      width = 4,
      import_googlesheets_ui("myid")
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

  imported <- import_googlesheets_server("myid")

  output$status <- renderPrint({
    imported$status()
  })
  output$data <- renderPrint({
    imported$data()
  })

}

if (interactive())
  shinyApp(ui, server)
