
# Select variables --------------------------------------------------------

library(shiny)
library(shinyWidgets)

data("mpg", package = "ggplot2")

ui <- fluidPage(
  fluidRow(
    column(
      width = 10, offset = 1,
      tags$h3("Filter data with select group module"),
      panel(
        checkboxGroupInput(
          inputId = "vars",
          label = "Variables to use:",
          choices = c("manufacturer", "model", "trans", "class"),
          selected = c("manufacturer", "model", "trans", "class"),
          inline = TRUE
        ),
        select_group_ui(
          id = "my-filters",
          params = list(
            manufacturer = list(inputId = "manufacturer", title = "Manufacturer:"),
            model = list(inputId = "model", title = "Model:"),
            trans = list(inputId = "trans", title = "Trans:"),
            class = list(inputId = "class", title = "Class:")
          ),
          inline = TRUE
        ),
        status = "primary"
      ),
      reactable::reactableOutput(outputId = "table")
    )
  )
)

server <- function(input, output, session) {

  vars_r <- reactive({
    input$vars
  })

  res_mod <- select_group_server(
    id = "my-filters",
    data = mpg,
    vars = vars_r
  )

  output$table <- reactable::renderReactable({
    reactable::reactable(res_mod())
  })
}

if (interactive())
  shinyApp(ui, server)
