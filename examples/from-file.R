

library(shiny)
library(datamods)

ui <- fluidPage(
  tags$h3("Import data from a file"),
  fluidRow(
    column(
      width = 4,
      import_file_ui(
        id = "myid",
        file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".json")
      )
    ),
    column(
      width = 8,
      tags$b("Imported data:"),
      verbatimTextOutput(outputId = "status"),
      verbatimTextOutput(outputId = "data")
    )
  )
)

server <- function(input, output, session) {

  imported <- import_file_server(
    id = "myid",
    # Custom functions to read data
    read_fns = list(
      xls = function(file, sheet, skip, encoding) {
        readxl::read_xls(path = file, sheet = sheet, skip = skip)
      },
      json = function(file) {
        jsonlite::read_json(file, simplifyVector = TRUE)
      }
    )
  )

  output$status <- renderPrint({
    imported$status()
  })
  output$data <- renderPrint({
    imported$data()
  })

}

if (interactive())
  shinyApp(ui, server)
