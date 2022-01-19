
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
    "Variable Manufacturer must be character",
    "Variable Model must be character",
    "Manufacturer X Model are unique",
    "Variable Price must be numeric",
    "Variable Min.Price must be numeric",
    "Variable Max.Price must be numeric",
    "Variable Price must be strictly positive",
    "Luggage.room must not contain any missing values",
    "Cylinders must be between 4 and 8",
    "Man.trans.avail must be 'Yes' or 'No'"
  )
  # you can also add a description()

  ui <- fluidPage(
    fluidRow(
      column(
        width = 4,
        checkboxGroupInput(
          inputId = "from",
          label = "From",
          choices = c("env", "file", "copypaste", "googlesheets", "api"),
          selected = c("file", "copypaste")
        ),
        actionButton("launch_modal", "Launch modal window")
      ),
      column(
        width = 8,
        tags$b("Imported data:"),
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
        title = "Import data to be used in application"
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
