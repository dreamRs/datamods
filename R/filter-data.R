
#' @title Shiny module to interactively filter a `data.frame`
#'
#' @description Module generate inputs to filter `data.frame` according column's type.
#'  Code to reproduce the filter is returned as an expression with filtered data.
#'
#' @param id Module id. See [shiny::callModule()].
#' @param show_nrow Show number of filtered rows and total.
#' @param max_height Maximum height for filters panel, useful
#'  if you have many variables to filter and limited space.
#'
#' @return
#' * UI: HTML tags that can be included in shiny's UI
#' * Server: a `list` with four slots:
#'   + **filtered**: a `reactive` function returning the data filtered.
#'   + **code**: a `reactive` function returning the dplyr pipeline to filter data.
#'   + **expr**: a `reactive` function returning an expression to filter data.
#'   + **values**: a `reactive` function returning a named list of variables and filter values.
#'
#' @export
#'
#' @name filter-data
#'
#' @importFrom htmltools tagList singleton tags validateCssUnit
#' @importFrom shiny NS uiOutput
#'
#' @example examples/filter_data.R
filter_data_ui <- function(id,
                           show_nrow = TRUE,
                           max_height = NULL) {
  ns <- NS(id)
  max_height <- if (!is.null(max_height)) {
    paste0("overflow-y: auto; overflow-x: hidden; max-height:", validateCssUnit(max_height), ";")
  }
  tagList(
    singleton(
      tags$style(
        ".selectize-big .selectize-input {height: 72px; overflow-y: scroll;}"
      )
    ),
    if (isTRUE(show_nrow)) {
      tags$span(i18n("Number of rows:"), uiOutput(outputId = ns("nrow"), inline = TRUE))
    },
    uiOutput(outputId = ns("placeholder_filters"), style = max_height)
  )
}

#' @param data [shiny::reactive()] function returning a
#'  \code{data.frame} to filter.
#' @param vars [shiny::reactive()] function returning a
#'  `character` vector of variables for which to add a filter.
#'  If a named `list`, names are used as labels.
#' @param name [shiny::reactive()] function returning a
#'  `character` string representing `data` name, only used for code generated.
#' @param defaults [shiny::reactive()] function returning a
#'  named `list` of variable:value pairs which will be used to set the filters.
#' @param drop_ids Drop columns containing more than 90% of unique values, or than 50 distinct values.
#' @param widget_char Widget to use for `character` variables: [shinyWidgets::pickerInput()]
#'  or [shiny::selectInput()] (default).
#' @param widget_num Widget to use for `numeric` variables: [shinyWidgets::numericRangeInput()]
#'  or [shiny::sliderInput()] (default).
#' @param widget_date Widget to use for `date/time` variables: [shiny::dateRangeInput()]
#'  or [shiny::sliderInput()] (default).
#' @param label_na Label for missing value widget.
#' @param value_na Default value for all NA's filters.
#'
#'
#' @rdname filter-data
#' @export
#'
#' @importFrom rlang eval_tidy %||%
#' @importFrom shiny observeEvent reactiveValues removeUI
#'  insertUI reactive req isolate reactive renderUI tags outputOptions
filter_data_server <- function(id,
                               data = reactive(NULL),
                               vars = reactive(NULL),
                               name = reactive("data"),
                               defaults = reactive(NULL),
                               drop_ids = TRUE,
                               widget_char = c("select", "picker"),
                               widget_num = c("slider", "range"),
                               widget_date = c("slider", "range"),
                               label_na = "NA",
                               value_na = TRUE) {
  widget_char <- match.arg(widget_char)
  widget_num <- match.arg(widget_num)
  widget_date <- match.arg(widget_date)
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      jns <- function(x) paste0("#", ns(x))

      output$nrow <- renderUI({
        tags$b(nrow(data_filtered()) , "/", nrow(data()))
      })

      rv_filters <- reactiveValues(mapping = NULL, mapping_na = NULL)
      rv_code <- reactiveValues(expr = NULL, dplyr = NULL)

      output$placeholder_filters <- renderUI({
        data <- data()
        req(data)
        vars <- vars()
        defaults <- defaults()
        filters <- create_filters(
          data = data,
          vars = vars,
          defaults = defaults,
          drop_ids = drop_ids,
          widget_char = widget_char,
          widget_num = widget_num,
          widget_date = widget_date,
          label_na = label_na,
          value_na = value_na
        )
        rv_filters$mapping <- filters$filters_id
        rv_filters$mapping_na <- filters$filters_na_id
        return(filters$ui)
      })
      
      filter_values <- reactive({
        data <- data()
        req(data)
        req(all(names(rv_filters$mapping) %in% names(data)))
        filter_inputs <- lapply(
          X = rv_filters$mapping,
          FUN = function(x) {
            input[[x]]
          }
        )
        filter_inputs
      })

      data_filtered <- reactive({
        data <- data()
        req(data)
        req(all(names(rv_filters$mapping) %in% names(data)))
        filter_inputs <- lapply(
          X = rv_filters$mapping,
          FUN = function(x) {
            # req(input[[x]])
            input[[x]]
          }
        )
        filter_nas <- lapply(
          X = rv_filters$mapping_na,
          FUN = function(x) {
            input[[x]]
          }
        )
        filters <- make_expr_filter(
          filters = filter_inputs,
          filters_na = filter_nas,
          data = data,
          data_name = isolate(name()) %||% "data"
        )
        rv_code$expr <- filters$expr
        rv_code$dplyr <- filters$expr_dplyr
        if (length(rv_code$expr) > 0) {
          result <- eval_tidy(expr = rv_code$expr, data = data)
          data[result, , drop = FALSE]
        } else {
          data
        }
      })
      outputOptions(x = output, name = "placeholder_filters", suspendWhenHidden = FALSE)

      return(list(
        filtered = data_filtered,
        values = filter_values,
        code = reactive(rv_code$dplyr),
        expr = reactive(rv_code$expr)
      ))
    }
  )
}



# Utils -------------------------------------------------------------------




#' @importFrom htmltools HTML tagList tags
#' @importFrom shiny selectizeInput sliderInput dateRangeInput
#' @importFrom stats setNames
#' @importFrom shinyWidgets pickerInput pickerOptions numericRangeInput
create_filters <- function(data,
                           vars = NULL,
                           defaults = NULL,
                           drop_ids = TRUE,
                           widget_char = c("select", "picker"),
                           widget_num = c("slider", "range"),
                           widget_date = c("slider", "range"),
                           label_na = "NA",
                           value_na = TRUE,
                           width = "100%",
                           session = getDefaultReactiveDomain()) {
  data <- as.data.frame(data)
  if (ncol(data) < 1)
    return(NULL)
  widget_char <- match.arg(widget_char)
  widget_num <- match.arg(widget_num)
  widget_date <- match.arg(widget_date)
  ns <- session$ns
  data <- drop_na(data)
  if (isTRUE(drop_ids)) {
    data <- drop_id(data)
  }
  data <- dropListColumns(data)
  if (is.null(vars)) {
    vars <- names(data)
    labels <- vars
  } else {
    if (rlang::is_named(vars)) {
      labels <- names(vars)
      vars <- unname(unlist(vars))
    } else {
      labels <- vars
    }
    vars <- intersect(names(data), vars)
  }
  # filters_id <- paste0("filter_", sample.int(1e9, length(vars)))
  filters_id <- paste0("filter_", makeId(vars))
  filters_id <- setNames(as.list(filters_id), vars)
  filters_na_id <- setNames(as.list(paste0("na_", filters_id)), vars)
  ui <- lapply(
    X = vars,
    FUN = function(variable) {
      var <- data[[variable]]
      any_na <- anyNA(var)
      var <- var[!is.na(var)]
      id <- filters_id[[variable]]
      label <- labels[variable == vars]

      tag_label <- tags$span(
        tags$label(
          label,
          class = "control-label",
          `for` = id
        ),
        HTML("&nbsp;&nbsp;"),
        if (any_na) na_filter(id = ns(paste0("na_", id)), label = label_na, value = value_na)
      )

      if (inherits(x = var, what = c("numeric", "integer"))) {
        params <- find_range_step(var)
        if(!is.null(defaults) && label %in% names(defaults)){
          params$range = defaults[[label]]
        }
        if (identical(widget_num, "slider")) {
          tags$div(
            style = "position: relative;",
            tag_label,
            set_slider_attr(sliderInput(
              inputId = ns(id),
              min = params$min,
              max = params$max,
              value = params$range,
              step = params$step,
              label = NULL,
              width = width
            ))
          )
        } else {
          tags$div(
            style = "position: relative;",
            tag_label,
            numericRangeInput(
              inputId = ns(id),
              value = params$range,
              label = NULL,
              width = width
            )
          )
        }
      } else if (inherits(x = var, what = c("Date", "POSIXct"))) {
        range_var <- range(var)
        if(!is.null(defaults) && label %in% names(defaults)){
          range_var = defaults[[label]]
        }
        if (identical(widget_date, "slider")) {
          tags$div(
            style = "position: relative;",
            tag_label,
            set_slider_attr(sliderInput(
              inputId = ns(id),
              min = min(var),
              max = max(var),
              value = range_var,
              label = NULL,
              width = width,
              timezone = if (inherits(var, "POSIXct")) format(var[1], format = "%z")
            ))
          )
        } else {
          tags$div(
            style = "position: relative;",
            tag_label,
            dateRangeInput(
              inputId = ns(id),
              min = min(var),
              max = max(var),
              start = range_var[1],
              end = range_var[2],
              label = NULL,
              width = width
            )
          )
        }
      } else {
        choices <- unique(as.character(var))
        if ("" %in% choices)
          choices <- append(choices, "<empty field>")
        choices <- tryCatch(choices[trimws(choices) != ""], error = function(e) {
          Encoding(choices[!validEnc(choices)]) <- "unknown"
          choices
        })
        selected = choices
        if(!is.null(defaults) && label %in% names(defaults)){
          selected = defaults[[label]]
        }
        if (identical(widget_char, "picker")) {
          tags$div(
            style = "position: relative;",
            tag_label,
            pickerInput(
              inputId = ns(id),
              choices = choices,
              selected = selected,
              label = NULL,
              multiple = TRUE,
              width = width,
              options = pickerOptions(
                actionsBox = TRUE,
                selectedTextFormat = "count",
                liveSearch = TRUE
              )
            )
          )
        } else {
          tags$div(
            style = "position: relative;",
            class = if (length(choices) > 15) "selectize-big",
            tag_label,
            selectizeInput(
              inputId = ns(id),
              choices = choices,
              selected = selected,
              label = NULL,
              multiple = TRUE,
              width = width,
              options = list(plugins = list("remove_button"))
            )
          )
        }
      }
    }
  )
  list(
    ui = tagList(ui),
    filters_id = filters_id,
    filters_na_id = filters_na_id
  )
}

tagSetAttributes <- function(tag, ...) {
  tag$attribs[names(list(...))] <- NULL
  tag$attribs <- c(tag$attribs, list(...))
  tag
}

set_slider_attr <- function(slider) {
  slider$children[[2]] <- tagSetAttributes(
    tag = slider$children[[2]],
    `data-force-edges` = "true",
    `data-grid-num` = "4"
  )
  slider
}

#' @importFrom htmltools tags
#' @importFrom shinyWidgets prettySwitch
na_filter <- function(id, label = "NA", value = TRUE) {
  tags$span(
    style = "position: absolute; right: 0px; margin-right: -20px;",
    prettySwitch(
      inputId = id,
      label = label,
      value = value,
      slim = TRUE,
      status = "primary",
      inline = TRUE
    )
  )
}


#' @importFrom rlang expr sym
make_expr_filter <- function(filters, filters_na, data, data_name) {
  expressions <- lapply(
    X = names(filters),
    FUN = function(var) {
      values <- filters[[var]]
      nas <- filters_na[[var]]
      data_values <- data[[var]]
      if (!is.null(values) & !match_class(values, data_values))
        return(NULL)
      values_expr <- NULL
      if (inherits(x = values, what = c("numeric", "integer"))) {
        data_range <- find_range_step(data_values)$range
        if (!isTRUE(all.equal(values, data_range))) {
          if (isTRUE(nas)) {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] | is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          } else {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] & !is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          }
        }
      } else if (inherits(x = values, what = c("Date", "POSIXct"))) {
        values <- if (inherits(values, "Date")) {
          format(values)
        } else {
          format(values, tz = format(data_values[1], format = "%Z"))
        }
        data_range <- range(data_values, na.rm = TRUE)
        data_range <- format(data_range)
        if (!identical(values, data_range)) {
          if (isTRUE(nas)) {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] | is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          } else {
            if (anyNA(data_values)) {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2] & !is.na(!!sym(var)))
            } else {
              values_expr <- expr(!!sym(var) >= !!values[1] & !!sym(var) <= !!values[2])
            }
          }
        }
      } else {
        data_values <- unique(as.character(data_values))
        if ("<empty field>" %in% values)
          values[which(values == "<empty field>")] <- ""
        if (!identical(sort(values), sort(data_values))) {
          if (length(values) == 0) {
            if (isTRUE(nas)) {
              values_expr <- expr(is.na(!!sym(var)))
            } else {
              values_expr <- expr(!(!!sym(var) %in% !!data_values[!is.na(data_values)]) & !is.na(!!sym(var)))
            }
          } else {
            if (length(values) <= length(data_values)/2) {
              if (isTRUE(nas)) {
                if (anyNA(data_values)) {
                  values_expr <- expr(!!sym(var) %in% !!values | is.na(!!sym(var)))
                } else {
                  values_expr <- expr(!!sym(var) %in% !!values)
                }
              } else {
                values_expr <- expr(!!sym(var) %in% !!values)
              }
            } else {
              if (isTRUE(nas)) {
                if (anyNA(data_values)) {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)) | is.na(!!sym(var)))
                } else {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)))
                }
              } else {
                if (anyNA(data_values)) {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)) & !is.na(!!sym(var)))
                } else {
                  values_expr <- expr(!(!!sym(var) %in% !!setdiff(data_values[!is.na(data_values)], values)))
                }
              }
            }
          }
        }
      }
      if (is.null(values_expr) & !isTRUE(nas) & anyNA(data_values)) {
        expr(!is.na(!!sym(var)))
      } else {
        values_expr
      }
    }
  )
  expressions <- dropNullsOrEmpty(expressions)
  data_name <- as.character(data_name)
  if (grepl("::", data_name)) {
    data_name <- str2lang(data_name)
  } else {
    data_name <- sym(data_name)
  }
  expr_dplyr <- Reduce(
    f = function(x, y) expr(!!x %>% filter(!!y)),
    x = expressions,
    init = expr(!!data_name)
  )
  expression <- Reduce(
    f = function(x, y) expr(!!x & !!y),
    x = expressions
  )
  return(list(
    expr_dplyr = expr_dplyr,
    expr = expression
  ))
}


drop_id <- function(data) {
  data[] <- lapply(
    X = data,
    FUN = function(x) {
      if (inherits(x, c("factor", "character"))) {
        values <- unique(as.character(x))
        values <- tryCatch(values[trimws(values) != ""], error = function(e){
          Encoding(values[!validEnc(values)]) <- "unknown"
          values
        })
        if (length(values) <= 1)
          return(NULL)
        if (length(values) >= length(x) * 0.9)
          return(NULL)
        if (length(values) >= 50)
          return(NULL)
      }
      x
    }
  )
  data
}

drop_na <- function(data) {
  data[] <- lapply(
    X = data,
    FUN = function(x) {
      if (all(is.na(x)))
        return(NULL)
      x
    }
  )
  data
}


# borrowed from shiny
hasDecimals <- function (value) {
  truncatedValue <- round(value)
  return(!identical(value, truncatedValue))
}

find_range_step <- function(x) {
  max <- max(x, na.rm = TRUE)
  min <- min(x, na.rm = TRUE)
  range <- max - min
  if (range < 2 || hasDecimals(min) || hasDecimals(max)) {
    pretty_steps <- pretty(c(min, max), n = 100, high.u.bias = 1)
    n_steps <- length(pretty_steps) - 1
    list(
      range = range(pretty_steps),
      min = min(pretty_steps),
      max = max(pretty_steps),
      step = signif(digits = 10, (max(pretty_steps) - min(pretty_steps))/n_steps)
    )
  }
  else {
    list(
      range = range(x, na.rm = TRUE),
      min = min,
      max = max,
      step = 1
    )
  }
}

match_class <- function(x, y) {
  char <- c("character", "factor")
  num <- c("numeric", "integer")
  date <- c("Date", "POSIXt")
  if (inherits(x, num) & inherits(y, num))
    return(TRUE)
  if (inherits(x, char) & inherits(y, char))
    return(TRUE)
  if (inherits(x, date) & inherits(y, date))
    return(TRUE)
  return(FALSE)
}

