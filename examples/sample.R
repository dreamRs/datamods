library(shiny)
library(echantillon)
library(shinythemes)

##### ui.R #####

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Sampling"),

  fluidRow(
    column(
      width = 4,
      sample_ui("myID")
    ),
    column(
      width = 8,
      dataTableOutput("table")
    )
  )
)



##### server.R #####

server <- function(input, output, session) {

  result_sample <- sample_server("myID", reactive(iris))

  output$table <- renderDataTable({
    result_sample()
  })
}

if (interactive())
  shinyApp(ui, server)

