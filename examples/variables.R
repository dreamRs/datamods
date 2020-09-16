
library(shiny)
library(DT)
library(data.table)
library(shinyWidgets)
library(htmltools)

ui <- fluidPage(
  tags$h3("Select, rename and convert variables"),
  fluidRow(
    column(
      width = 5,
      # radioButtons()
      update_variables_ui("vars")
    ),
    column(
      width = 7,
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output, session) {

  updated_data <- update_variables_server(
    id = "vars",
    data = reactive(dplyr::starwars)
  )


  output$result <- renderPrint({
    updated_data()
  })
}

shinyApp(ui, server)
