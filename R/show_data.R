
#' Display a table in a window
#'
#' @param data a data object (either a \code{matrix} or a \code{data.frame}).
#' @param title Title to be displayed in window.
#' @param show_classes Show variables classes under variables names in table header.
#' @param type Display table in a pop-up or in modal window.
#' @param options Options passed to \link[DT]{datatable}'s options argument.
#' @param width Width of the window, only used if \code{type = "popup"}.
#'
#' @return No value.
#' @export
#'
#' @importFrom shinyWidgets show_alert
#' @importFrom htmltools tags tagList css
#' @importFrom DT datatable renderDT tableHeader
#' @importFrom shiny showModal modalDialog
#'
#' @example examples/show_data.R
show_data <- function(data,
                      title = NULL,
                      options = NULL,
                      show_classes = TRUE,
                      type = c("popup", "modal"),
                      width = "80%") { # nocov start
  type <- match.arg(type)
  data <- as.data.frame(data)
  if (isTRUE(show_classes)) {
    classes <- get_classes(data)
    classes <- sprintf("<span style='font-style: italic; font-weight: normal; font-size: small;'>%s</span>", classes)
    container <- tags$table(
      tableHeader(paste(paste0(names(data), "&nbsp;&nbsp;"), classes, sep = "<br>"), escape = FALSE)
    )
  } else {
    container <- tags$table(
      tableHeader(paste0(names(data), "&nbsp;&nbsp;"), escape = FALSE)
    )
  }
  datatable <- DT::datatable(
    data = data,
    rownames = FALSE,
    selection = "none",
    class = "display dt-responsive cell-border compact",
    style = "bootstrap",
    width = "100%",
    container = container,
    options = c(list(
      scrollX = TRUE
    ), options)
  )
  if (identical(type, "popup")) {
    show_alert(
      title = NULL,
      text = tags$div(
        if (!is.null(title)) {
          tagList(
            tags$h3(title),
            tags$hr()
          )
        },
        style = "color: #000 !important;",
        tags$style(".dataTables_length {text-align: left;}"),
        tags$style(".dataTables_info {text-align: left;}"),
        datatable
      ),
      closeOnClickOutside = TRUE,
      showCloseButton = TRUE,
      btn_labels = NA,
      html = TRUE,
      width = width
    )
  } else {
    showModal(modalDialog(
      title = tagList(
        tags$button(
          phosphoricons::ph("x", title = i18n("Close"), height = "2em"),
          class = "btn btn-link",
          style = css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
          `data-dismiss` = "modal",
          `aria-label` = i18n("Close")
        ),
        title
      ),
      DT::renderDT(datatable),
      size = "l",
      footer = NULL
    ))
  }
} # nocov end


