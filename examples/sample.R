library(shiny)
library(datamods)
library(reactable)


ui <- fluidPage(

  tags$h2("Sampling"),

  fluidRow(
    column(
      width = 3,
      sample_ui("myID")
    ),
    column(
      width = 9,
      reactableOutput("table")
    )
  )
)


server <- function(input, output, session) {

  result_sample <- sample_server("myID", reactive(iris))

  output$table <- renderReactable({
    table_sample <- reactable(
      data = result_sample(),
      defaultColDef = colDef(
        align = "center"
      ),
      borderless = TRUE,
      highlight = TRUE,
      striped = TRUE
    )
    return(table_sample)
  })
}

if (interactive())
  shinyApp(ui, server)

