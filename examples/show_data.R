
library(shiny)
library(datamods)

ui <- fluidPage(
  actionButton(
    inputId = "show",
    label = "Show data",
    icon = icon("eye")
  )
)

server <- function(input, output, session) {
  observeEvent(input$show, {
    show_data(mtcars, title = "My data")
  })
}

if (interactive())
  shinyApp(ui, server)
