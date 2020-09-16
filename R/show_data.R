
#' Display a table in a window
#'
#' @param data a data object (either a \code{matrix} or a \code{data.frame}).
#' @param title Title to be displayed in window.
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
show_data <- function(data, title = "Imported data", width = "80%") {
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
        options = list(
          scrollX = TRUE
        )
      )
    ),
    showCloseButton = TRUE,
    btn_labels = "Close window",
    html = TRUE,
    width = width
  )
}

