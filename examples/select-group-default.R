# Default -----------------------------------------------------------------

library(shiny)
library(shinyWidgets)

data("mpg", package = "ggplot2")

ui <- fluidPage(
  fluidRow(
    column(
      width = 10, offset = 1,
      tags$h3("Filter data with select group module"),
      shinyWidgets::panel(
        select_group_ui(
          id = "my-filters",
          params = list(
            manufacturer = list(inputId = "manufacturer", title = "Manufacturer:"),
            model = list(inputId = "model", title = "Model:"),
            trans = list(inputId = "trans", title = "Trans:"),
            class = list(inputId = "class", title = "Class:")
          )
        ),
        status = "primary"
      ),
      reactable::reactableOutput(outputId = "table")
    )
  )
)

server <- function(input, output, session) {
  res_mod <- select_group_server(
    id = "my-filters",
    data = reactive(mpg),
    vars = reactive(c("manufacturer", "model", "trans", "class"))
  )
  output$table <- reactable::renderReactable({
    reactable::reactable(res_mod())
  })
}

if (interactive())
  shinyApp(ui, server)
