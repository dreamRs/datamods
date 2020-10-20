
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
  tags$h3("Select, rename and convert variables"),
  fluidRow(
    column(
      width = 6,
      # radioButtons()
      update_variables_ui("vars")
    ),
    column(
      width = 6,
      verbatimTextOutput("result"),
      verbatimTextOutput("str")
    )
  )
)

server <- function(input, output, session) {

  updated_data <- update_variables_server(
    id = "vars",
    data = reactive(testdata)
  )

  output$result <- renderPrint({
    updated_data()
  })
  output$str <- renderPrint({
    str(updated_data())
  })
}

if (interactive())
  shinyApp(ui, server)
