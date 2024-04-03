
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
                      width = "65%") { # nocov start
  type <- match.arg(type)
  data <- as.data.frame(data)

  gridTheme <- getOption("datagrid.theme")
  if (length(gridTheme) < 1) {
    apply_grid_theme()
  }
  on.exit(toastui::reset_grid_theme())

  if (is.null(options))
    options <- list()

  options$height <- 550
  options$minBodyHeight <- 400
  options$data <- data
  options$theme <- "default"
  options$colwidths <- "guess"
  options$guess_colwidths_opts <- list(min_width = 90, max_width = 400, mul = 1, add = 10)
  if (isTRUE(show_classes))
    options$summary <- construct_col_summary(data)
  datatable <- rlang::exec(toastui::datagrid, !!!options)
  datatable <- toastui::grid_columns(datatable, className = "font-monospace")
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
      tags$div(
        style = css(minHeight = validateCssUnit(options$height)),
        toastui::renderDatagrid2(datatable)
        # datatasble
      ),
      size = "l",
      footer = NULL,
      easyClose = TRUE
    ))
  }
} # nocov end

