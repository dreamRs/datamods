library(shiny)

##### ui.R #####

ui <- fluidPage(
  # theme = shinythemes::shinytheme("cerulean"),
  # Thème bslib
  theme = bslib::bs_theme (
    version = 5,
    bg = "#FFFFFF",
    fg = "#0f3587",
    primary = "#0f3587",
    "well-bg" = "#FFF",
    base_font = font_google("Poppins"),
    code_font = font_google("Poppins")
  ),

  titlePanel("Sampling"),

  fluidRow(
    column(
      width = 4,
      sample_ui("myID")
    ),
    column(
      width = 8,
      #dataTableOutput("table")
      reactableOutput("table")
    )
  )
)



##### server.R #####

server <- function(input, output, session) {

  result_sample <- sample_server("myID", reactive(iris))

  # output$table <- renderDataTable({
  #   result_sample()
  # })

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

