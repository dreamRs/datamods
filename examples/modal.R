
library(shiny)
library(datamods)

ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      checkboxGroupInput(inputId = "from",
                         label = "From",
                         choices = c("env", "file", "copypaste", "googlesheets")),
      actionButton("launch_modal", "Launch Modal(s)")
    ),
    column(
      width = 8,
      tags$b("Imported data:"),
      verbatimTextOutput(outputId = "result")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$launch_modal, {
    req(input$from)
    import_modal("myid", from = input$from)
  })
  
  imported <- import_server("myid")
  
  output$result <- renderPrint({
    req(imported())
    imported()
  })
}

if (interactive())
  shinyApp(ui, server)
