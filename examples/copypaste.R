
library(shiny)

# Application

ui <- fluidPage(
  tags$h3("Import data by Pasting"),
  fluidRow(
    column(
      width = 4,
      mod_import_copypaste_ui("myid")
    ),
    column(
      width = 8,
      tags$b("Imported data:"),
      verbatimTextOutput(outputId = "result")
    )
  )
)

server <- function(input, output, session) {
  
  imported <- callModule(mod_import_copypaste_server, "myid")
  
  output$result <- renderPrint({
    imported$data()
  })
  
}

if (interactive())
  shinyApp(ui, server)
