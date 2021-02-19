
#' @title Validation module
#'
#' @description Check that a dataset respect some validation expectations.
#'
#' @param id Module's ID.
#' @param display Display validation results in a dropdown menu
#'  by clicking on a button or display results directly in interface.
#' @param max_height Maximum height for validation results element, useful if you have many rules.
#' @param ... Arguments passed to \code{actionButton} or \code{uiOutput} depending on display mode,
#'  you cannot use \code{inputId}/\code{outputId}, \code{label} or \code{icon} (button only).
#'
#' @return
#'  * UI: HTML tags that can be included in shiny's UI
#'  * Server: a \code{list} with two slots:
#'    + **status**: a \code{reactive} function returning the best status available between \code{"OK"}, \code{"Failed"} or \code{"Error"}.
#'    + **details**: a \code{reactive} function returning a \code{list} with validation details.
#' @export
#'
#' @importFrom shiny NS actionButton icon uiOutput
#' @importFrom htmltools tagList validateCssUnit
#' @importFrom shinyWidgets dropMenu
#'
#' @rdname validation
#'
#' @example examples/validation.R
validation_ui <- function(id, display = c("dropdown", "inline"), max_height = NULL, ...) {
  ns <- NS(id)
  display <- match.arg(display)
  max_height <- if (!is.null(max_height)) {
    paste0("overflow-y: auto; max-height:", validateCssUnit(max_height), ";")
  }
  if (identical(display, "dropdown")) {
    ui <- dropMenu(
      actionButton(
        inputId = ns("menu"),
        label = i18n("Validation:"),
        ...,
        icon = icon("caret-down")
      ),
      uiOutput(
        outputId = ns("results"),
        style = "width: 300px;",
        style = max_height
      )
    )
  } else {
    ui <- uiOutput(
      outputId = ns("results"),
      ...,
      style = max_height
    )
  }
  tagList(
    ui, html_dependency_datamods()
  )
}

#' @export
#'
#' @param data a \code{reactive} function returning a \code{data.frame}.
#' @param n_row,n_col A one-sided formula to check number of rows and columns respectively, see below for examples.
#' @param n_row_label,n_col_label Text to be displayed with the result of the check for number of rows/columns.
#' @param btn_label Label for the dropdown button, will be followed by validation result.
#' @param rules An object of class \code{validator} created with \code{validate::validator}.
#'
#' @rdname validation
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent updateActionButton renderUI reactive
#' @importFrom htmltools doRenderTags tags tagList
validation_server <- function(id,
                              data,
                              n_row = NULL,
                              n_col = NULL,
                              n_row_label = "Valid number of rows",
                              n_col_label = "Valid number of columns",
                              btn_label = "Dataset validation:",
                              rules = NULL) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      valid_ui <- reactiveValues(x = NULL)

      valid_rv <- reactiveValues(status = NULL, details = NULL)

      observeEvent(data(), {
        to_validate <- data()
        valid_dims <- check_data(to_validate, n_row = n_row, n_col = n_col)

        if (all(c(valid_dims$nrows, valid_dims$ncols))) {
          valid_status <- "OK"
        } else {
          valid_status <- "Failed"
        }

        valid_results <- lapply(
          X = c("nrows", "ncols"),
          FUN = function(x) {
            if (is.null(valid_dims[[x]]))
              return(NULL)
            label <- switch(
              x,
              "nrows" = n_row_label,
              "ncols" = n_col_label
            )
            list(
              status = ifelse(valid_dims[[x]], "OK", "Failed"),
              label = paste0("<b>", label, "</b>")
            )
          }
        )

        if (!is.null(rules) && inherits(rules, "validator")) {
          validate_results <- validate::confront(to_validate, rules)
          validate_results <- validate::summary(validate_results)
          validate_results <- merge(
            x = validate_results,
            y = validate::as.data.frame(rules),
            by = "name"
          )
          # validate_results <- format_validate(validate_results)
          if (any(validate_results$error)) {
            valid_status <- "Error"
          } else if (any(validate_results$fails > 0)) {
            valid_status <- "Failed"
          }
          valid_results <- c(
            valid_results,
            format_validate(validate_results)
          )
        }

        if (identical(valid_status, "OK")) {
          label <- doRenderTags(tagList(
            btn_label,
            tags$span(
              class = "label label-success",
              icon("check"), i18n("OK")
            )
          ))
        } else if (identical(valid_status, "Failed")) {
          label <- doRenderTags(tagList(
            btn_label,
            tags$span(
              class = "label label-warning",
              icon("warning"), i18n("Failed")
            )
          ))
        } else if (identical(valid_status, "Error")) {
          label <- doRenderTags(tagList(
            btn_label,
            tags$span(
              class = "label label-danger",
              icon("times"), i18n("Error")
            )
          ))
        }
        updateActionButton(session = session, inputId = "menu", label = label)

        valid_results <- dropNulls(valid_results)

        total <- unlist(lapply(valid_results, `[[`, "status"))

        header <- tags$div(
          class = "datamods-validation-results",
          tags$div(
            class = "datamods-validation-summary",
            style = "border-right: 1px solid #e6e6e6;",
            tags$span(
              class = "label label-success",
              i18n("OK"),
              tags$span(sum(total == "OK"), class = "datamods-validation-item")
            )
          ),
          tags$div(
            class = "datamods-validation-summary",
            style = "border-right: 1px solid #e6e6e6;",
            tags$span(
              class = "label label-warning",
              i18n("Failed"),
              tags$span(sum(total == "Failed"), class = "datamods-validation-item")
            )
          ),
          tags$div(
            class = "datamods-validation-summary",
            tags$span(
              class = "label label-danger",
              i18n("Error"),
              tags$span(sum(total == "Error"), class = "datamods-validation-item")
            )
          )
        )

        valid_ui$x <- tagList(
          header,
          tags$br(),
          make_validation_alerts(valid_results)
        )

        valid_rv$status <- valid_status
        valid_rv$details <- valid_results
      })

      output$results <- renderUI({
        valid_ui$x
      })

      return(list(
        status = reactive(valid_rv$status),
        details = reactive(valid_rv$details)
      ))
    }
  )
}

#' @importFrom rlang as_label as_function enquo
check_fun <- function(fun, what) {
  label <- as_label(enquo(what))
  if (inherits(fun, "formula")) {
    fun <- as_function(fun)
    result <- try(fun(what))
    if (inherits(result, "try-error") | !is.logical(result)) {
      warning("Checking ", label, " must return a logical", call. = FALSE)
      return(FALSE)
    }
  } else {
    result <- NULL
  }
  return(result)
}

check_data <- function(data, n_row = NULL, n_col = NULL) {
  list(
    nrows = check_fun(n_row, nrow(data)),
    ncols = check_fun(n_col, ncol(data))
  )
}


#' @importFrom shiny icon
#' @importFrom shinyWidgets alert
#' @importFrom htmltools HTML
make_validation_alerts <- function(.list) {
  lapply(
    X = .list,
    FUN = function(x) {
      icon <- switch(
        x$status,
        "OK" = icon("check"),
        "Failed" = icon("warning"),
        "Error" = icon("times")
      )
      status <- switch(
        x$status,
        "OK" = "success",
        "Failed" = "warning",
        "Error" = "danger",
        "info"
      )
      alert(
        icon, HTML(x$label),
        status = status,
        style = "margin-bottom: 10px; padding: 10px;"
      )
    }
  )
}


format_validate <- function(data) {
  lapply(
    X = seq_len(nrow(data)),
    FUN = function(i) {
      res <- data[i, ]
      if (isTRUE(res$error)) {
        status <- "Error"
      } else {
        if (res$fails > 0) {
          status <- "Failed"
        } else {
          status <- "OK"
        }
      }
      if (!is.null(res$label)) {
        label <- paste0("<b>", res$label, "</b>")
        if (!is.null(res$description) && nzchar(res$description)) {
          label <- paste(label, res$description, sep = ": ")
        }
      } else {
        label <- res$name
      }
      if (identical(status, "Failed")) {
        label <- paste0(label, "| failed: ", res$fails, " / ", res$items)
      }
      list(
        status = status,
        label = label,
        summary = res
      )
    }
  )
}


