
#' Display a table in a window
#'
#' @param data a data object (either a \code{matrix} or a \code{data.frame}).
#' @param title Title to be displayed in window.
#' @param options Options passed to \link[DT]{datatable}'s options argument.
#' @param width Width of the window.
#'
#' @return No value.
#' @export
#'
#' @importFrom shinyWidgets show_alert
#' @importFrom htmltools tags
#' @importFrom DT datatable
#'
#' @example examples/show_data.R
show_data <- function(data, title = "Imported data", options = NULL, width = "80%") { # nocov start
  data <- as.data.frame(data)
  show_alert(
    title = NULL,
    text = tags$div(
      tags$h3(title),
      tags$hr(),
      style = "color: #000 !important;",
      tags$style(".dataTables_length {text-align: left;}"),
      tags$style(".dataTables_info {text-align: left;}"),
      DT::datatable(
        data = data,
        rownames = FALSE,
        selection = "none",
        class = "display dt-responsive",
        style = "bootstrap",
        width = "100%",
        autoHideNavigation = TRUE,
        options = c(list(
          scrollX = TRUE
        ), options)
      )
    ),
    closeOnClickOutside = TRUE,
    showCloseButton = TRUE,
    btn_labels = NA,
    html = TRUE,
    width = width
  )
} # nocov end


