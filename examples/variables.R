
library(shiny)
library(datamods)

testdata <- data.frame(
  date_as_char = as.character(Sys.Date() + 0:9),
  date_as_num = as.numeric(Sys.Date() + 0:9),
  datetime_as_char = as.character(Sys.time() + 0:9 * 3600*24),
  datetime_as_num = as.numeric(Sys.time() + 0:9 * 3600*24),
  num_as_char = as.character(1:10),
  char = month.name[1:10],
  char_na = c("A", "A", "B", NA, "B", "A", NA, "B", "A", "B"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5L, preset = "bootstrap"),
  tags$h3("Select, rename and convert variables"),
  fluidRow(
    column(
      width = 6,
      # radioButtons()
      update_variables_ui("vars")
    ),
    column(
      width = 6,
      tags$b("original data:"),
      verbatimTextOutput("original"),
      verbatimTextOutput("original_str"),
      tags$b("Modified data:"),
      verbatimTextOutput("modified"),
      verbatimTextOutput("modified_str")
    )
  )
)

server <- function(input, output, session) {

  updated_data <- update_variables_server(
    id = "vars",
    data = reactive(testdata),
    return_data_on_init = FALSE
  )

  output$original <- renderPrint({
    testdata
  })
  output$original_str <- renderPrint({
    str(testdata)
  })

  output$modified <- renderPrint({
    updated_data()
  })
  output$modified_str <- renderPrint({
    str(updated_data())
  })
}

if (interactive())
  shinyApp(ui, server)
