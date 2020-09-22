
# Import data from a package -------------------------------------

library(shiny)
library(datamods)

# /!\ Works only if there's no data.frame in environment

ui <- fluidPage(
  tags$h3("Import data from Global Environment"),
  fluidRow(
    column(
      width = 4,
      import_globalenv_ui(id = "myid")
    ),
    column(
      width = 8,
      tags$b("Imported data:"),
      verbatimTextOutput(outputId = "result")
    )
  )
)

server <- function(input, output, session) {

  imported <- import_globalenv_server(
    id = "myid",
    choices = list_pkg_data("ggplot2")
  )

  output$result <- renderPrint({
    imported$data()
  })

}

if (interactive())
  shinyApp(ui, server)
