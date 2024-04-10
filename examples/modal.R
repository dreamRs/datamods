
library(shiny)
library(datamods)

ui <- fluidPage(
  # Try with different Bootstrap version
  # theme = bslib::bs_theme(version = 5, preset = "bootstrap"),
  fluidRow(
    column(
      width = 4,
      checkboxGroupInput(
        inputId = "from",
        label = i18n("From"),
        choices = c("env", "file", "copypaste", "googlesheets", "url"),
        selected = c("file", "copypaste")
      ),
      actionButton("launch_modal", i18n("Launch modal window"))
    ),
    column(
      width = 8,
      tags$b(i18n("Imported data:")),
      verbatimTextOutput(outputId = "name"),
      verbatimTextOutput(outputId = "data"),
      verbatimTextOutput(outputId = "str_data")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$launch_modal, {
    req(input$from)
    import_modal(
      id = "myid",
      from = input$from,
      title = i18n("Import data to be used in application")
    )
  })

  imported <- import_server("myid", return_class = "tbl_df")

  output$name <- renderPrint({
    req(imported$name())
    imported$name()
  })

  output$data <- renderPrint({
    req(imported$data())
    imported$data()
  })

  output$str_data <- renderPrint({
    req(imported$data())
    str(imported$data())
  })

}

if (interactive())
  shinyApp(ui, server)
