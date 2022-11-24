
# Select variables --------------------------------------------------------

library(shiny)
library(datamods)
library(shinyWidgets)

ui <- fluidPage(
  fluidRow(
    column(
      width = 10, offset = 1,
      tags$h3("Filter data with select group module"),
      panel(
        checkboxGroupInput(
          inputId = "vars",
          label = "Variables to use:",
          choices = c("Manufacturer", "Type", "AirBags", "DriveTrain"),
          selected = c("Manufacturer", "Type", "AirBags", "DriveTrain"),
          inline = TRUE
        ),
        select_group_ui(
          id = "my-filters",
          params = list(
            list(inputId = "Manufacturer", label = "Manufacturer:"),
            list(inputId = "Type", label = "Type:"),
            list(inputId = "AirBags", label = "AirBags:"),
            list(inputId = "DriveTrain", label = "DriveTrain:")
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
    data = MASS::Cars93,
    vars = vars_r
  )

  output$table <- reactable::renderReactable({
    reactable::reactable(res_mod())
  })
}

if (interactive())
  shinyApp(ui, server)
