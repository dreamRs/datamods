
#' Display a table in a window
#'
#' @param data a data object (either a `matrix` or a `data.frame`).
#' @param title Title to be displayed in window.
#' @param show_classes Show variables classes under variables names in table header.
#' @param type Display table in a pop-up with [shinyWidgets::show_alert()],
#'  in modal window with [shiny::showModal()] or in a WinBox window with [shinyWidgets::WinBox()].
#' @param options Arguments passed to [toastui::datagrid()].
#' @param width Width of the window, only used if `type = "popup"` or `type = "winbox"`.
#' @param ... Additional options, such as `wbOptions = wbOptions()` or `wbControls = wbControls()`.
#'
#' @note
#' If you use `type = "winbox"`, you'll need to use `shinyWidgets::html_dependency_winbox()` somewhere in your UI.
#'
#' @return No value.
#' @export
#'
#' @importFrom shinyWidgets show_alert
#' @importFrom htmltools tags tagList css
#' @importFrom shiny showModal modalDialog
#' @importFrom utils modifyList packageVersion
#'
#' @example examples/show_data.R
show_data <- function(data,
                      title = NULL,
                      options = NULL,
                      show_classes = TRUE,
                      type = c("popup", "modal", "winbox"),
                      width = "65%",
                      ...) { # nocov start
  type <- match.arg(type)
  data <- as.data.frame(data)
  args <- list(...)
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
  if (identical(type, "winbox")) {
    stopifnot(
      "You need shinyWidgets >= 0.8.4" = packageVersion("shinyWidgets") >= "0.8.4"
    )
    wb_options <- if (is.null(args$wbOptions)) {
      shinyWidgets::wbOptions(
        height = "600px",
        width = width,
        modal = TRUE
      )
    } else {
      modifyList(
        shinyWidgets::wbOptions(
          height = "600px",
          width = width,
          modal = TRUE
        ),
        args$wbOptions
      )
    }
    wb_controls <- if (is.null(args$wbControls)) {
      shinyWidgets::wbControls()
    } else {
      args$wbControls
    }
    shinyWidgets::WinBox(
      title = title,
      ui = datatable,
      options = wb_options,
      controls = wb_controls,
      padding = "0 5px"
    )
  } else if (identical(type, "popup")) {
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
        button_close_modal(),
        title
      ),
      tags$div(
        style = css(minHeight = validateCssUnit(options$height)),
        toastui::renderDatagrid2(datatable)
      ),
      size = "xl",
      footer = NULL,
      easyClose = TRUE
    ))
  }
} # nocov end

