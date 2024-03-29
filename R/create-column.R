
#' @title Create new column
#'
#' @description
#' This module allow to enter an expression to create a new column in `data.frame`.
#'
#'
#' @param id Module's ID.
#'
#' @return A [shiny::reactive()] function returning the data.
#'
#' @note User can only use a subset of function: `r paste(list_allowed_operations(), collapse=", ")`.
#'  You can add more operations using the `allowed_operations` argument, for  example if you want to allow to use package lubridate, you can do:
#'  ```r
#'  c(list_allowed_operations(), getNamespaceExports("lubridate"))
#'  ```
#'
#' @export
#'
#' @importFrom htmltools tagList tags css
#' @importFrom shiny NS textInput textAreaInput uiOutput actionButton
#' @importFrom phosphoricons ph
#'
#' @name create-column
#'
#' @example examples/create_column.R
create_column_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(
      inputId = ns("new_column"),
      label = "New column name:",
      value = "new_column1",
      width = "100%"
    ),
    textAreaInput(
      inputId = ns("expression"),
      label = "Enter an expression to define new column:",
      value = "",
      width = "100%",
      rows = 6
    ),
    tags$i(
      class = "d-block",
      ph("info"),
      "Click on a column to add it to the expression:"
    ),
    uiOutput(outputId = ns("columns")),
    tags$div(
      style = css(
        display = "grid",
        gridTemplateColumns = "3fr 1fr",
        columnGap = "10px",
        margin = "10px 0"
      ),
      actionButton(
        inputId = ns("compute"),
        label = tagList(
          ph("gear"), "Create column"
        ),
        class = "btn-outline-primary",
        width = "100%"
      ),
      actionButton(
        inputId = ns("remove"),
        label = tagList(
          ph("trash")
        ),
        class = "btn-outline-danger",
        width = "100%"
      )
    ),
    uiOutput(outputId = ns("feedback"))
  )
}

#' @param data_r A [shiny::reactive()] function returning a `data.frame`.
#' @param allowed_operations A `list` of allowed operations, see below for details.
#'
#' @export
#'
#' @rdname create-column
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent renderUI req
#'  updateTextAreaInput reactive
#' @importFrom shinyWidgets alert
create_column_server <- function(id,
                                 data_r = reactive(NULL),
                                 allowed_operations = list_allowed_operations()) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rv <- reactiveValues(data = NULL, feedback = NULL)

      observeEvent(data_r(), rv$data <- data_r())

      output$feedback <- renderUI(rv$feedback)

      output$columns <- renderUI({
        data <- req(rv$data)
        lapply(names(data), btn_column, inputId = ns("add_column"))
      })

      observeEvent(input$add_column, {
        updateTextAreaInput(
          session = session,
          inputId = "expression",
          value = paste0(input$expression, input$add_column)
        )
      })

      observeEvent(input$new_column, {
        if (input$new_column == "") {
          rv$feedback <- alert(
            status = "warning",
            ph("warning"), "New column name cannot be empty"
          )
        }
      })

      observeEvent(input$remove, {
        rv$data[[input$new_column]] <- NULL
      })
      observeEvent(input$compute, {
        rv$feedback <- try_compute_column(
          expression = input$expression,
          name = input$new_column,
          rv = rv,
          allowed_operations = allowed_operations
        )
      })

      return(reactive(rv$data))
    }
  )
}

#' @export
#'
#' @rdname create-column
# @importFrom methods getGroupMembers
list_allowed_operations <- function() {
  c(
    "(",
    # getGroupMembers("Arith"),
    c("+", "-", "*", "^", "%%", "%/%", "/"),
    # getGroupMembers("Compare"),
    c("==", ">", "<", "!=", "<=", ">="),
    # getGroupMembers("Logic"),
    c("&", "|"),
    # getGroupMembers("Math"),
    c("abs", "sign", "sqrt", "ceiling", "floor", "trunc", "cummax",
      "cummin", "cumprod", "cumsum", "exp", "expm1", "log", "log10",
      "log2", "log1p", "cos", "cosh", "sin", "sinh", "tan", "tanh",
      "acos", "acosh", "asin", "asinh", "atan", "atanh", "cospi", "sinpi",
      "tanpi", "gamma", "lgamma", "digamma", "trigamma"),
    # getGroupMembers("Math2"),
    c("round", "signif"),
    # getGroupMembers("Summary"),
    c("max", "min", "range", "prod", "sum", "any", "all"),
    "pmin", "pmax", "mean",
    "paste", "paste0", "substr", "nchar", "trimws",
    "gsub", "sub", "grepl", "ifelse"
  )
}


#' @importFrom rlang parse_expr eval_tidy
try_compute_column <- function(expression, name, rv, allowed_operations) {
  parsed <- try(parse(text = expression, keep.source = FALSE), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    return(alert_error(attr(parsed, "condition")$message))
  }
  funs <- unlist(c(extract_calls(parsed), lapply(parsed, extract_calls)), recursive = TRUE)
  if (!are_allowed_operations(funs, allowed_operations)) {
    return(alert_error("Some operations are not allowed"))
  }
  result <- try(
    eval_tidy(parse_expr(expression), data = rv$data),
    silent = TRUE
  )
  if (inherits(result, "try-error")) {
    return(alert_error(attr(result, "condition")$message))
  }
  adding_col <- try(rv$data[[name]] <- result, silent = TRUE)
  if (inherits(adding_col, "try-error")) {
    return(alert_error(attr(adding_col, "condition")$message))
  }
  alert(
    status = "success",
    ph("check"), "Column added!"
  )
}

are_allowed_operations <- function(x, allowed_operations) {
  all(
    x %in% allowed_operations
  )
}


extract_calls <- function(exp) {
  if (is.call(exp))
    return(list(
      as.character(exp[[1L]]),
      lapply(exp[-1L], extract_calls)
    ))
}

alert_error <- function(text) {
  alert(
    status = "danger",
    ph("bug"), text
  )
}


btn_column <- function(label, inputId) {
  tags$button(
    type = "button",
    class = "btn btn-primary",
    style = css(
      "--bs-btn-padding-y" = ".25rem",
      "--bs-btn-padding-x" = ".5rem",
      "--bs-btn-font-size" = ".75rem",
      "margin-bottom" = "5px"
    ),
    label,
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      inputId, label
    )
  )
}

