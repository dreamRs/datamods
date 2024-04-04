
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
#' @importFrom shinyWidgets virtualSelectInput
#'
#' @name create-column
#'
#' @example examples/create_column.R
create_column_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        textInput(
          inputId = ns("new_column"),
          label = "New column name:",
          value = "new_column1",
          width = "100%"
        )
      ),
      column(
        width = 6,
        virtualSelectInput(
          inputId = ns("group_by"),
          label = "Group calculation by:",
          choices = NULL,
          multiple = TRUE,
          disableSelectAll = TRUE,
          hasOptionDescription = TRUE,
          width = "100%"
        )
      )
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
#'  updateTextAreaInput reactive bindEvent observe
#' @importFrom shinyWidgets alert updateVirtualSelect
create_column_server <- function(id,
                                 data_r = reactive(NULL),
                                 allowed_operations = list_allowed_operations()) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      rv <- reactiveValues(data = NULL, feedback = NULL)

      bindEvent(observe({
        data <- data_r()
        updateVirtualSelect(
          inputId = "group_by",
          choices = make_choices_with_infos(data)
        )
      }), data_r(), input$hidden)

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
          allowed_operations = allowed_operations,
          by = input$group_by
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
    "(", "c",
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
    "gsub", "sub", "grepl", "ifelse", "length",
    "as.numeric", "as.character", "as.integer", "as.Date", "as.POSIXct",
    "as.factor", "factor"
  )
}


#' @inheritParams shiny::modalDialog
#' @export
#'
#' @importFrom shiny showModal modalDialog
#' @importFrom htmltools tagList
#'
#' @rdname create-column
modal_create_column <- function(id,
                                title = "Create a new column",
                                easyClose = TRUE,
                                size = "l",
                                footer = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(title, button_close_modal()),
    create_column_ui(id),
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = genId())
    ),
    easyClose = easyClose,
    size = size,
    footer = footer
  ))
}


#' @importFrom rlang parse_expr eval_tidy call2 set_names syms
#' @importFrom data.table as.data.table :=
try_compute_column <- function(expression,
                               name,
                               rv,
                               allowed_operations,
                               by = NULL) {
  parsed <- try(parse(text = expression, keep.source = FALSE), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    return(alert_error(attr(parsed, "condition")$message))
  }
  funs <- unlist(c(extract_calls(parsed), lapply(parsed, extract_calls)), recursive = TRUE)
  if (!are_allowed_operations(funs, allowed_operations)) {
    return(alert_error("Some operations are not allowed"))
  }
  if (!isTruthy(by)) {
    result <- try(
      eval_tidy(parse_expr(expression), data = rv$data),
      silent = TRUE
    )
  } else {
    result <- try(
      {
        dt <- as.data.table(rv$data)
        new_col <- NULL
        dt[, new_col := eval_tidy(parse_expr(expression), data = .SD), by = by]
        dt$new_col
      },
      silent = TRUE
    )
  }
  if (inherits(result, "try-error")) {
    return(alert_error(attr(result, "condition")$message))
  }
  adding_col <- try(rv$data[[name]] <- result, silent = TRUE)
  if (inherits(adding_col, "try-error")) {
    return(alert_error(attr(adding_col, "condition")$message))
  }
  code <- if (!isTruthy(by)) {
    call2("mutate", !!!set_names(list(parse_expr(expression)), name))
  } else {
    expr(
      !!expr(group_by(!!!syms(by))) %>%
        !!call2("mutate", !!!set_names(list(parse_expr(expression)), name))
    )
  }
  attr(rv$data, "code") <- c(attr(rv$data, "code"),  code)
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


#' @importFrom data.table uniqueN
#' @importFrom htmltools doRenderTags
make_choices_with_infos <- function(data) {
  lapply(
    X = seq_along(data),
    FUN = function(i) {
      nm <- names(data)[i]
      values <- data[[nm]]
      icon <- if (inherits(values, "character")) {
        phosphoricons::ph("text-aa")
      } else if (inherits(values, "factor")) {
        phosphoricons::ph("list-bullets")
      } else if (inherits(values, c("numeric", "integer"))) {
        phosphoricons::ph("hash")
      } else if (inherits(values, c("Date"))) {
        phosphoricons::ph("calendar")
      } else if (inherits(values, c("POSIXt"))) {
        phosphoricons::ph("clock")
      } else {
        NULL
      }
      list(
        label = htmltools::doRenderTags(tagList(
          icon, nm
        )),
        value = nm,
        description = paste("Unique values:", data.table::uniqueN(values))
      )
    }
  )
}

