
# Import data from a database connection -------------------------------------

library(shiny)
library(datamods)

# Create a connection

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(con, "mtcars", mtcars)
DBI::dbWriteTable(con, "iris", iris)

ui <- fluidPage(
  tags$h3("Import data from a database"),
  fluidRow(
    column(
      width = 4,
      import_database_ui("myid")
    ),
    column(
      width = 8,
      tags$b("Imported data:"),
      verbatimTextOutput(outputId = "result")
    )
  )
)

server <- function(input, output, session) {
  
  imported <- import_database_server("myid", input)
  
  output$result <- renderPrint({
    imported$data()
  })
  
}

if (interactive())
  shinyApp(ui, server)
