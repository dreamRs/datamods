
library(shiny)
library(datamods)

ui <- fluidPage(
  tags$h3(i18n("Import data from Googlesheets")),
  fluidRow(
    column(
      width = 4,
      import_googlesheets_ui("myid")
    ),
    column(
      width = 8,
      tags$b(i18n("Import status:")),
      verbatimTextOutput(outputId = "status"),
      tags$b(i18n("Name:")),
      verbatimTextOutput(outputId = "name"),
      tags$b(i18n("Data:")),
      verbatimTextOutput(outputId = "data")
    )
  )
)

server <- function(input, output, session) {

  imported <- import_googlesheets_server("myid")

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
