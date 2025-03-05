

# Selected value --------------------------------------------------------------------

library(shiny)
library(datamods)

ui <- fluidPage(
  select_group_ui(
    id = "my-filters",
    params = list(
      list(inputId = "Manufacturer", label = "Manufacturer:"),
      list(inputId = "Type", label = "Type:")
    ),
    vs_args = list(
      disableSelectAll = FALSE
    )
  ),
  actionButton("set_sel", "Set Manufacturer=Acura"),
  verbatimTextOutput("res")
)

server <- function(input, output, session) {
  # We use a reactiveValue so that it can be updated
  rv <- reactiveValues(selected = list(Manufacturer = "Audi")) # for init
  res_r <- select_group_server(
    id = "my-filters",
    data = reactive(MASS::Cars93),
    vars = reactive(c("Manufacturer", "Type")),
    selected_r = reactive(rv$selected)
  )
  output$res <- renderPrint({
    res_r()
  })
  observeEvent(input$set_sel, {
    rv$selected <- list(Manufacturer = "Acura")
  })
}

if (interactive())
  shinyApp(ui, server)
