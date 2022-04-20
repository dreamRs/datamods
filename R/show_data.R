
#' Display a table in a window
#'
#' @param data a data object (either a `matrix` or a `data.frame`).
#' @param title Title to be displayed in window.
#' @param show_classes Show variables classes under variables names in table header.
#' @param type Display table in a pop-up or in modal window.
#' @param options Arguments passed to [reactable::reactable()].
#' @param width Width of the window, only used if `type = "popup"`.
#'
#' @return No value.
#' @export
#'
#' @importFrom shinyWidgets show_alert
#' @importFrom htmltools tags tagList css
#' @importFrom shiny showModal modalDialog
#' @importFrom utils modifyList
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
    defaultColDef <- reactable::colDef(
      header = function(value) {
        if (!value %in% names(data))
          return("")
        classes <- tags$div(
          style = "font-style: italic; font-weight: normal; font-size: small;",
          get_classes(data[, value, drop = FALSE])
        )
        tags$div(title = value, value, classes)
      }
    )
  } else {
    defaultColDef <- NULL
  }
  if (is.null(options))
    options <- list()
  options <- modifyList(x = options, val = list(
    bordered = TRUE,
    compact = TRUE,
    striped = TRUE
  ))
  options$data <- data
  options$defaultColDef <- defaultColDef
  datatable <- rlang::exec(reactable::reactable, !!!options)
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
          `data-bs-dismiss` = "modal",
          `aria-label` = i18n("Close")
        ),
        title
      ),
      reactable::renderReactable(datatable),
      size = "l",
      footer = NULL,
      easyClose = TRUE
    ))
  }
} # nocov end


