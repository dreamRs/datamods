
library(shiny)
library(datamods)
library(ggplot2)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5L, preset = "bootstrap"),
  shinyWidgets::html_dependency_winbox(),
  tags$h2("Reorder the Levels of a Factor"),
  fluidRow(
    column(
      width = 6,
      update_factor_ui("id"),
      actionButton("modal", "Or click here to open a modal to update factor's level"),
      tags$br(), tags$br(),
      actionButton("winbox", "Or click here to open a WinBox to create a column")
    ),
    column(
      width = 6,
      selectInput(
        "var",
        label = "Variable to plot:",
        choices = NULL
      ),
      plotOutput("plot"),
      verbatimTextOutput("res")
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(data = MASS::Cars93[c(1, 2, 3, 9, 10, 11, 16, 26, 27)])
  observe(
    updateSelectInput(inputId = "var", choices = names(rv$data))
  )

  # Inline mode
  data_inline_r <- update_factor_server(
    id = "id",
    data_r = reactive(rv$data)
  )
  observeEvent(data_inline_r(), rv$data <- data_inline_r())

  # modal window mode
  observeEvent(input$modal, modal_update_factor("modal"))
  data_modal_r <- update_factor_server(
    id = "modal",
    data_r = reactive(rv$data)
  )
  observeEvent(data_modal_r(), {
    shiny::removeModal()
    rv$data <- data_modal_r()
  })

  # winbox mode
  observeEvent(input$winbox, winbox_update_factor("winbox"))
  data_winbox_r <- update_factor_server(
    id = "winbox",
    data_r = reactive(rv$data)
  )
  observeEvent(data_winbox_r(), rv$data <- data_winbox_r())

  # Plot results
  output$plot <- renderPlot({
    req(input$var, rv$data)
    ggplot(rv$data) +
      aes(x = !!sym(input$var)) +
      geom_bar()
  })
  # Show results
  output$res <- renderPrint({
    data <- req(rv$data)
    str(data)
  })
}

if (interactive())
  shinyApp(ui, server)
