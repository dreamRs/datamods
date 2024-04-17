
library(shiny)
library(datamods)
library(reactable)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5L, preset = "bootstrap"),
  shinyWidgets::html_dependency_winbox(),
  tags$h2("Convert Numeric to Factor"),
  fluidRow(
    column(
      width = 6,
      cut_variable_ui("inline"),
      actionButton("modal", "Or click here to open a modal to cut a variable"),
      tags$br(), tags$br(),
      actionButton("winbox", "Or click here to open a WinBox to cut a variable")
    ),
    column(
      width = 6,
      reactableOutput(outputId = "table"),
      verbatimTextOutput("code")
    )
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(data = MASS::Cars93[, c(1, 3, 4, 5, 6, 10)])

  # inline mode
  data_inline_r <- cut_variable_server(
    id = "inline",
    data_r = reactive(rv$data)
  )
  observeEvent(data_inline_r(), rv$data <- data_inline_r())

  # modal window mode
  observeEvent(input$modal, modal_cut_variable("modal"))
  data_modal_r <- cut_variable_server(
    id = "modal",
    data_r = reactive(rv$data)
  )
  observeEvent(data_modal_r(), rv$data <- data_modal_r())

  # WinBox window mode
  observeEvent(input$winbox, winbox_cut_variable("winbox"))
  data_winbox_r <- cut_variable_server(
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
