
# Subset data -------------------------------------------------------------

library(shiny)
library(datamods)
library(shinyWidgets)


ui <- fluidPage(
  fluidRow(
    column(
      width = 10, offset = 1,
      tags$h3("Filter data with select group module"),
      panel(
        pickerInput(
          inputId = "car_select",
          choices = unique(MASS::Cars93$Manufacturer),
          options = list(
            `live-search` = TRUE,
            title = "None selected"
          )
        ),
        select_group_ui(
          id = "my-filters",
          params = list(
            list(inputId = "Manufacturer", label = "Manufacturer:"),
            list(inputId = "Type", label = "Type:"),
            list(inputId = "AirBags", label = "AirBags:"),
            list(inputId = "DriveTrain", label = "DriveTrain:")
          )
        ),
        status = "primary"
      ),
      reactable::reactableOutput(outputId = "table")
    )
  )
)

server <- function(input, output, session) {

  cars_r <- reactive({
    subset(MASS::Cars93, Manufacturer %in% input$car_select)
  })

  res_mod <- select_group_server(
    id = "my-filters",
    data = cars_r,
    vars = c("Manufacturer", "Type", "AirBags", "DriveTrain")
  )

  output$table <- reactable::renderReactable({
    reactable::reactable(res_mod())
  })
}

if (interactive())
  shinyApp(ui, server)
