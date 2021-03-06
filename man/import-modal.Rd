% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import-modal.R
\name{import-modal}
\alias{import-modal}
\alias{import_ui}
\alias{import_server}
\alias{import_modal}
\title{Get all import functions in a Modal}
\usage{
import_ui(id, from = c("env", "file", "copypaste", "googlesheets"))

import_server(
  id,
  validation_opts = NULL,
  allowed_status = c("OK", "Failed", "Error"),
  return_class = c("data.frame", "data.table", "tbl_df")
)

import_modal(id, from, title = "Import data", size = "l")
}
\arguments{
\item{id}{Module's id}

\item{from}{The import_ui & server to use, i.e. the method.
There are 4 options to choose from. ("env", "file", "copypaste", "googlesheets")}

\item{validation_opts}{\code{list} of arguments passed to \code{\link{validation_server}}.}

\item{allowed_status}{Vector of statuses allowed to confirm dataset imported,
if you want that all validation rules are successful before importing data use \code{allowed_status = "OK"}.}

\item{return_class}{Class of returned data: \code{data.frame}, \code{data.table} or \code{tbl_df} (tibble).}

\item{title}{Modal window title.}

\item{size}{Modal window size, default to \code{"l"} (large).}
}
\value{
\itemize{
\item UI: HTML tags that can be included in shiny's UI
\item Server: a \code{list} with two slots:
\itemize{
\item \strong{data}: a \code{reactive} function returning the imported \code{data.frame}.
\item \strong{name}: a \code{reactive} function returning the name of the imported data as \code{character} (if applicable).
}
}
}
\description{
Let the user choose a method of his choice to import data
}
\examples{

library(shiny)
library(datamods)

ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      checkboxGroupInput(
        inputId = "from",
        label = "From",
        choices = c("env", "file", "copypaste", "googlesheets"),
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

  imported <- import_server("myid", return_class = "tbl_df")

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
