
library(shiny)
library(datamods)

if (requireNamespace("validate")) {
  library(validate)
  # Define some rules to be applied to data
  myrules <- validator(
    is.character(Manufacturer) | is.factor(Manufacturer),
    is.character(Model) | is.factor(Model),
    is_unique(Manufacturer, Model),
    is.numeric(Price),
    is.numeric(Min.Price),
    is.numeric(Max.Price),
    Price > 12, # we should use 0 for testing positivity, but that's for the example
    !is.na(Luggage.room),
    in_range(Cylinders, min = 4, max = 8),
    Man.trans.avail %in% c("Yes", "No")
  )
  # Add some labels
  label(myrules) <- c(
    i18n("Variable Manufacturer must be character"),
    i18n("Variable Model must be character"),
    i18n("Manufacturer X Model are unique"),
    i18n("Variable Price must be numeric"),
    i18n("Variable Min.Price must be numeric"),
    i18n("Variable Max.Price must be numeric"),
    i18n("Variable Price must be strictly positive"),
    i18n("Luggage.room must not contain any missing values"),
    i18n("Cylinders must be between 4 and 8"),
    i18n("Man.trans.avail must be 'Yes' or 'No'")
  )
  # you can also add a description()

  ui <- fluidPage(
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
        verbatimTextOutput(outputId = "data")
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

    imported <- import_server(
      id = "myid",
      return_class = "tbl_df",
      validation_opts = list(
        # rules = validator(.file = system.file("extdata/rules.yaml", package = "datamods"))
        rules = myrules
      )
    )

    output$name <- renderPrint({
      req(imported$name())
      imported$name()
    })

    output$data <- renderPrint({
      req(imported$data())
      imported$data()
    })
  }

  if (interactive())
    shinyApp(ui, server)
}
