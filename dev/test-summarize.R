
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

try_fun <- function(fun, x, ...) {
  tryCatch({
    fun <- match.fun(fun)
    fun(x, ...)
  }, error = function(e) NA)
}
funs <- list(
  "sum" = function(x) try_fun("sum", x, na.rm = TRUE),
  "mean" = function(x) try_fun("mean", x, na.rm = TRUE),
  "min" = function(x) try_fun("min", x, na.rm = TRUE),
  "max" = function(x) try_fun("max", x, na.rm = TRUE)
)
# funs <- lapply(funs, as_function)


# Utils -------------------------------------------------------------------

compute_by <- function(data, by, funs = NULL, on = NULL, add_count = TRUE) {
  data <- as.data.table(data)
  if (is.null(by))
    return(data)
  if (is.null(funs) | is.null(on)) {
    if (isTRUE(add_count)) {
      return(data[, list(count = .N), by = by])
    } else {
      return(unique(data[, .SD, .SDcols = by]))
    }
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
    if (isTRUE(add_count))
      results <- c(list(data[, list(count = .N), by = by]), results)
    Reduce(function(x, y) {
      merge(x, y, by = by)
    }, results)
  }
}

# compute_by(mpg, by = "manufacturer")
# compute_by(mpg, by = "manufacturer", add_count = FALSE)
# 
# compute_by(mpg, by = "manufacturer", on = "cty")
# compute_by(mpg, by = "manufacturer", on = "cty", add_count = FALSE)
# 
# compute_by(mpg, by = "manufacturer", on = "cty", funs = funs[1:2])
# compute_by(mpg, by = "manufacturer", on = "cty", funs = funs[1:2], add_count = FALSE)




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
      checkboxInput(
        inputId = "add_count", 
        label = "Add count?",
        value = TRUE
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
    compute_by(
      data_, by = input$by, 
      funs = funs, 
      on = input$on,
      add_count = input$add_count
    )
  })
  
  output$table <- renderTable({
    summarized_r()
  }, striped = TRUE, bordered = TRUE, width = "100%")
  
}

# shinyApp(ui, server)
runGadget(app = shinyApp(ui, server), viewer = dialogViewer("Summarize", width = 1000))
