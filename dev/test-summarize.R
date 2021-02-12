
library(shiny)
library(htmltools)
library(miniUI)
library(shinyWidgets)
library(tidyr)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(rlang)


funs <- list(
  "sum" = ~ sum(.x, na.rm = TRUE),
  "mean" = ~ mean(.x, na.rm = TRUE),
  "min" = ~ min(.x, na.rm = TRUE),
  "max" = ~ max(.x, na.rm = TRUE)
)
funs <- lapply(funs, as_function)


# Utils -------------------------------------------------------------------

compute_by <- function(data, by, funs = NULL, on = NULL) {
  data <- as.data.table(data)
  if (is.null(by))
    return(data)
  if (is.null(funs) | is.null(on)) {
    return(data[, list(count = .N), by = by])
  }
  if (identical(length(funs), 1L)) {
    return(data[, lapply(.SD, funs[[1]]), by = by, .SDcols = on])
  } else {
    results <- lapply(
      X = seq_along(funs),
      FUN = function(i) {
        fun <- funs[[i]]
        fun_nm <- names(funs)[i]
        new <- paste(on, fun_nm, sep = "_")
        result <- data[, lapply(.SD, fun), by = by, .SDcols = on]
        setnames(result, on, new)
        result[]
      }
    )
    Reduce(function(x, y) {
      merge(x, y, by = by)
    }, results)
  }
}




# APP ---------------------------------------------------------------------

ui <- fluidPage(
  tags$h3("Summarize data", style = "text-align: center;"),

  fluidRow(
    column(
      width = 3,
      pickerInput(
        inputId = "by",
        label = "Group by:",
        multiple = TRUE,
        choices = NULL
      ),
      pickerInput(
        inputId = "on",
        label = "Compute on:",
        multiple = TRUE,
        choices = NULL
      )
    ),
    column(
      width = 9,
      tagAppendAttributes(
        tableOutput("table"),
        style = "margin-top: 10px; overflow: auto; max-height: 500px; word-break: keep-all; white-space: nowrap;"
      )
    )
  )
)

server <- function(input, output, session) {

  data_r <- reactive({mpg})

  observeEvent(data_r(), {
    names_ <- names(data_r())
    updatePickerInput(
      session = session,
      inputId = "by",
      choices = names_
    )
    updatePickerInput(
      session = session,
      inputId = "on",
      choices = names_
    )
  })

  summarized_r <- reactive({
    data_ <- data_r()
    # data_ <- as.data.table(data_)
    # if (is.null(input$by)) {
    #   return(data_)
    # }
    # if (is.null(input$on)) {
    #   return(data_[, list(count = .N), by = c(input$by)])
    # }
    # data_
    compute_by(data_, by = input$by, funs = funs, on = input$on)
  })

  output$table <- renderTable({
    summarized_r()
  }, striped = TRUE, bordered = TRUE, width = "100%")

}

# shinyApp(ui, server)
runGadget(app = shinyApp(ui, server), viewer = dialogViewer("Summarize", width = 1000))
