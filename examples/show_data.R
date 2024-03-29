
library(shiny)
library(datamods)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5L),
  actionButton(
    inputId = "show1",
    label = "Show data in popup",
    icon = icon("eye")
  ),
  actionButton(
    inputId = "show2",
    label = "Show data in modal",
    icon = icon("eye")
  ),
  actionButton(
    inputId = "show3",
    label = "Show data without classes",
    icon = icon("eye")
  )
)

server <- function(input, output, session) {
  observeEvent(input$show1, {
    show_data(mtcars, title = "My data")
  })
  observeEvent(input$show2, {
    show_data(mtcars, title = "My data", type = "modal")
  })
  observeEvent(input$show3, {
    show_data(
      data = mtcars,
      title = "My data",
      show_classes = FALSE,
      options = list(pagination = 10),
      type = "modal"
    )
  })
}

if (interactive())
  shinyApp(ui, server)
