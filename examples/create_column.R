
library(shiny)
library(datamods)
library(reactable)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5L, preset = "bootstrap"),
  shinyWidgets::html_dependency_winbox(),
  tags$h2("Create new column"),
  fluidRow(
    column(
      width = 4,
      create_column_ui("inline"),
      actionButton("modal", "Or click here to open a modal to create a column"),
      tags$br(), tags$br(),
      actionButton("winbox", "Or click here to open a WinBox to create a column")
    ),
    column(
      width = 8,
      reactableOutput(outputId = "table"),
      verbatimTextOutput("code")
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(data = MASS::Cars93[, c(1, 3, 4, 5, 6, 10)])

  # inline mode
  data_inline_r <- create_column_server(
    id = "inline",
    data_r = reactive(rv$data)
  )
  observeEvent(data_inline_r(), rv$data <- data_inline_r())

  # modal window mode
  observeEvent(input$modal, modal_create_column("modal"))
  data_modal_r <- create_column_server(
    id = "modal",
    data_r = reactive(rv$data)
  )
  observeEvent(data_modal_r(), rv$data <- data_modal_r())

  # WinBox window mode
  observeEvent(input$winbox, winbox_create_column("winbox"))
  data_winbox_r <- create_column_server(
    id = "winbox",
    data_r = reactive(rv$data)
  )
  observeEvent(data_winbox_r(), rv$data <- data_winbox_r())

  # Show result
  output$table <- renderReactable({
    data <- req(rv$data)
    reactable(
      data = data,
      bordered = TRUE,
      compact = TRUE,
      striped = TRUE
    )
  })

  output$code <- renderPrint({
    attr(rv$data, "code")
  })
}

if (interactive())
  shinyApp(ui, server)
