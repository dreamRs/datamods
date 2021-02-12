library(shiny)
library(shinyWidgets)
library(datamods)
library(MASS)

# Add some NAs to mpg
mtcars_na <- mtcars
mtcars_na[] <- lapply(
  X = mtcars_na,
  FUN = function(x) {
    x[sample.int(n = length(x), size = sample(15:30, 1))] <- NA
    x
  }
)

datetime <- data.frame(
  date = seq(Sys.Date(), by = "day", length.out = 300),
  datetime = seq(Sys.time(), by = "hour", length.out = 300),
  num = sample.int(1e5, 300)
)

ui <- fluidPage(
  tags$h2("Filter data.frame"),

  radioButtons(
    inputId = "dataset",
    label = "Data:",
    choices = c(
      "iris",
      "mtcars",
      "mtcars_na",
      "Cars93",
      "datetime"
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
      tags$b("Code dplyr:"),
      verbatimTextOutput(outputId = "code_dplyr"),
      tags$b("Expression:"),
      verbatimTextOutput(outputId = "code"),
      tags$b("Filtered data:"),
      verbatimTextOutput(outputId = "res_str")
    )
  )
)

server <- function(input, output, session) {

  data <- reactive({
    get(input$dataset)
  })

  vars <- reactive({
    if (identical(input$dataset, "mtcars")) {
      setNames(as.list(names(mtcars)[1:5]), c(
        "Miles/(US) gallon",
        "Number of cylinders",
        "Displacement (cu.in.)",
        "Gross horsepower",
        "Rear axle ratio"
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
    widget_date = "slider",
    label_na = "Missing"
  )

  observeEvent(res_filter$filtered(), {
    updateProgressBar(
      session = session, id = "pbar",
      value = nrow(res_filter$filtered()), total = nrow(data())
    )
  })

  output$table <- DT::renderDT({
    res_filter$filtered()
  }, options = list(pageLength = 5))


  output$code_dplyr <- renderPrint({
    res_filter$code()
  })
  output$code <- renderPrint({
    res_filter$expr()
  })

  output$res_str <- renderPrint({
    str(res_filter$filtered())
  })

}

if (interactive())
  shinyApp(ui, server)
