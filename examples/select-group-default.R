# Default -----------------------------------------------------------------

library(shiny)
library(datamods)
library(shinyWidgets)


ui <- fluidPage(
  # theme = bslib::bs_theme(version = 5L),
  fluidRow(
    column(
      width = 10, offset = 1,
      tags$h3(i18n("Filter data with select group module")),
      shinyWidgets::panel(
        select_group_ui(
          id = "my-filters",
          params = list(
            list(inputId = "Manufacturer", label = i18n("Manufacturer:")),
            list(inputId = "Type", label = i18n("Type:")),
            list(inputId = "AirBags", label = i18n("AirBags:")),
            list(inputId = "DriveTrain", label = i18n("DriveTrain:"))
          ), vs_args = list(disableSelectAll = FALSE)
        ),
        status = "primary"
      ),
      reactable::reactableOutput(outputId = "table"),
      tags$b(i18n("Inputs values:")),
      verbatimTextOutput("inputs")
    )
  )
)

server <- function(input, output, session) {
  res_mod <- select_group_server(
    id = "my-filters",
    data = reactive(MASS::Cars93),
    vars = reactive(c("Manufacturer", "Type", "AirBags", "DriveTrain"))
  )

  output$table <- reactable::renderReactable({
    reactable::reactable(res_mod())
  })

  output$inputs <- renderPrint({
    attr(res_mod(), "inputs")
  })
}

if (interactive())
  shinyApp(ui, server)
