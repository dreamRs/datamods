
library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  actionButton(
    inputId = "show",
    label = "Show data",
    icon = icon("eye")
  )
)

server <- function(input, output, session) {
  observeEvent(input$show, {
    show_data(mtcars)
  })
}

if (interactive())
  shinyApp(ui, server)
