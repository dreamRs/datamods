library(datamods)
library(shiny)

if (requireNamespace("validate")) {
  library(validate)

  # Define some rules to be applied to data
  myrules <- validator(
    is.character(Manufacturer) | is.factor(Manufacturer),
    is.numeric(Price),
    Price > 12, # we should use 0 for testing positivity, but that's for the example
    !is.na(Luggage.room),
    in_range(Cylinders, min = 4, max = 8),
    Man.trans.avail %in% c("Yes", "No")
  )
  # Add some labels
  label(myrules) <- c(
    "Variable Manufacturer must be character",
    "Variable Price must be numeric",
    "Variable Price must be strictly positive",
    "Luggage.room must not contain any missing values",
    "Cylinders must be between 4 and 8",
    "Man.trans.avail must be 'Yes' or 'No'"
  )
  # you can also add a description()

  ui <- fluidPage(
    tags$h2("Validation"),
    fluidRow(
      column(
        width = 4,
        radioButtons(
          inputId = "dataset",
          label = "Choose dataset:",
          choices = c("mtcars", "MASS::Cars93")
        ),
        tags$p("Dropdown example:"),
        validation_ui("validation1"),

        tags$br(),

        tags$p("Inline example:"),
        validation_ui("validation2", display = "inline")
      ),
      column(
        width = 8,
        tags$b("Status:"),
        verbatimTextOutput("status"),
        tags$b("Details:"),
        verbatimTextOutput("details")
      )
    )
  )

  server <- function(input, output, session) {

    dataset <- reactive({
      if (input$dataset == "mtcars") {
        mtcars
      } else {
        MASS::Cars93
      }
    })

    results <- validation_server(
      id = "validation1",
      data = dataset,
      n_row = ~ . > 20, # more than 20 rows
      n_col = ~ . >= 3, # at least 3 columns
      rules = myrules
    )

    validation_server(
      id = "validation2",
      data = dataset,
      n_row = ~ . > 20, # more than 20 rows
      n_col = ~ . >= 3, # at least 3 columns
      rules = myrules
    )

    output$status <- renderPrint(results$status())
    output$details <- renderPrint(results$details())

  }

  if (interactive())
    shinyApp(ui, server)
}
