library(shiny)
library(shinyWidgets)
library(ggplot2)
library(esquisse)

# Add some NAs to mpg
mpg_na <- mpg
mpg_na[] <- lapply(
  X = mpg_na,
  FUN = function(x) {
    x[sample.int(n = length(x), size = sample(15:30, 1))] <- NA
    x
  }
)

ui <- fluidPage(
  tags$h2("Filter data.frame"),

  radioButtons(
    inputId = "dataset",
    label = "Data:",
    choices = c(
      "iris", "mtcars", "economics",
      "midwest", "mpg", "mpg_na", "msleep", "diamonds",
      "faithfuld", "txhousing"
    ),
    inline = TRUE
  ),

  fluidRow(
    column(
      width = 3,
      filter_data_ui("filtering", max_height = "500px")
    ),
    column(
      width = 9,
      progressBar(
        id = "pbar", value = 100,
        total = 100, display_pct = TRUE
      ),
      DT::dataTableOutput(outputId = "table"),
      tags$p("Code dplyr:"),
      verbatimTextOutput(outputId = "code_dplyr"),
      tags$p("Expression:"),
      verbatimTextOutput(outputId = "code"),
      tags$p("Filtered data:"),
      verbatimTextOutput(outputId = "res_str")
    )
  )
)

server <- function(input, output, session) {

  data <- reactive({
    get(input$dataset)
  })

  vars <- reactive({
    if (identical(input$dataset, "mpg")) {
      setNames(as.list(names(mpg)[1:5]), c(
        "Manufacturer name",
        "Model name",
        "Engine displacement, in litres",
        "Year of manufacture",
        "Number of cylinders"
      ))
    } else {
      NULL
    }
  })

  res_filter <- filter_data_server(
    id = "filtering",
    data = data,
    name = reactive(input$dataset),
    vars = vars,
    widget_num = "slider",
    widget_date = "range",
    label_na = "Missing"
  )

  observeEvent(res_filter$data_filtered(), {
    updateProgressBar(
      session = session, id = "pbar",
      value = nrow(res_filter$data_filtered()), total = nrow(data())
    )
  })

  output$table <- DT::renderDT({
    res_filter$data_filtered()
  }, options = list(pageLength = 5))


  output$code_dplyr <- renderPrint({
    res_filter$code$dplyr
  })
  output$code <- renderPrint({
    res_filter$code$expr
  })

  output$res_str <- renderPrint({
    str(res_filter$data_filtered())
  })

}

shinyApp(ui, server)
