
library(shiny)
library(datamods)
library(ggplot2)
library(stringr)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5L, preset = "bootstrap"),
  tags$h2("Reorder the Levels of a Factor"),
  fluidRow(
    column(
      width = 6,
      update_factors_ui("id")
    ),
    column(
      width = 6,
      verbatimTextOutput("code")
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(data = MASS::Cars93[c(1, 2, 3, 9, 10, 11, 16, 26, 27)]) 
  
  data_inline_r <- update_factors_server(
    id = "id",
    data_r = reactive(rv$data)
  )
  observeEvent(data_inline_r(), rv$data <- data_inline_r())
  
  # Show result
  output$code <- renderPrint({
    data <- req(rv$data)
    data %>% str()
  })
}

if (interactive())
  shinyApp(ui, server)
