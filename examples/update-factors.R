
library(shiny)
library(datamods)
library(ggplot2)
library(ggpubr)
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
      plotOutput(outputId = "graph"),
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
  output$graph <- renderPlot({
    data <- req(rv$data)
    names_cols_updated <- str_subset(names(data), pattern = "_updated$")
    
    if (identical(names_cols_updated, character(0))) {
      ggplot()
    } else {
      listes_graphiques <- lapply(
        X = str_subset(names(data), pattern = "_updated$"),
        FUN = function(x) {
          ggplot(data) +
            geom_bar(aes(x = .data[[x]]), fill = "#112466") +
            theme_minimal() +
            labs(y = NULL)
        }
      )
      ggarrange(ncol = 1, plotlist = listes_graphiques)
    }
  })
  
  output$code <- renderPrint({
    data <- req(rv$data)
    data %>% str()
  })
}

if (interactive())
  shinyApp(ui, server)
