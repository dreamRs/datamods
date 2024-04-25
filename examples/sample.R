library(shiny)
library(reactable)

##### ui.R #####

ui <- fluidPage(

  titlePanel(i18n("Sampling")),

  fluidRow(
    column(
      width = 4,
      sample_ui("myID")
    ),
    column(
      width = 8,
      reactableOutput("table")
    )
  )
)



##### server.R #####

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

