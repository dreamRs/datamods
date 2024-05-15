
#' @title Module to Convert Numeric to Factor
#'
#' @description
#' This module contain an interface to cut a numeric into several intervals.
#'
#'
#' @param id Module ID.
#'
#' @return A [shiny::reactive()] function returning the data.
#' @export
#'
#' @importFrom shiny NS fluidRow column numericInput checkboxInput checkboxInput plotOutput
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom toastui datagridOutput2
#'
#' @name cut-variable
#'
#' @example examples/cut_variable.R
cut_variable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        virtualSelectInput(
          inputId = ns("variable"),
          label = i18n("Variable to cut:"),
          choices = NULL,
          width = "100%"
        )
      ),
      column(
        width = 3,
        virtualSelectInput(
          inputId = ns("method"),
          label = i18n("Method:"),
          choices = c(
            "sd",
            "equal",
            "pretty",
            "quantile",
            # "kmeans",
            # "hclust",
            # "bclust",
            # "fisher",
            # "jenks",
            "headtails",
            "maximum",
            "box"
          ),
          selected = "quantile",
          width = "100%"
        )
      ),
      column(
        width = 3,
        numericInput(
          inputId = ns("n_breaks"),
          label = i18n("Number of breaks:"),
          value = 5,
          min = 2,
          max = 12,
          width = "100%"
        )
      ),
      column(
        width = 3,
        checkboxInput(
          inputId = ns("right"),
          label = i18n("Close intervals on the right"),
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("include_lowest"),
          label = i18n("Include lowest value"),
          value = FALSE
        )
      )
    ),
    plotOutput(outputId = ns("plot"), width = "100%", height = "270px"),
    datagridOutput2(outputId = ns("count")),
    actionButton(
      inputId = ns("create"),
      label = tagList(ph("scissors"), i18n("Create factor variable")),
      class = "btn-outline-primary float-end"
    ),
    tags$div(class = "clearfix")
  )
}

#' @param data_r A [shiny::reactive()] function returning a `data.frame`.
#'
#' @export
#'
#' @importFrom shiny moduleServer observeEvent reactive req bindEvent renderPlot
#' @importFrom shinyWidgets updateVirtualSelect
#' @importFrom toastui renderDatagrid2 datagrid grid_colorbar
#' @importFrom rlang %||% call2 set_names expr syms
#' @importFrom classInt classIntervals
#'
#' @rdname cut-variable
cut_variable_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(data = NULL)

      bindEvent(observe({
        data <- data_r()
        rv$data <- data
        vars_num <- vapply(data, is.numeric, logical(1))
        vars_num <- names(vars_num)[vars_num]
        updateVirtualSelect(
          inputId = "variable",
          choices = vars_num,
          selected = if (isTruthy(input$variable)) input$variable else vars_num[1]
        )
      }), data_r(), input$hidden)

      breaks_r <- reactive({
        data <- req(data_r())
        variable <- req(input$variable)
        req(hasName(data, variable))
        req(input$n_breaks, input$method)
        classInt::classIntervals(data[[variable]], n = input$n_breaks, style = input$method)
      })

      output$plot <- renderPlot({
        data <- req(data_r())
        variable <- req(input$variable)
        # ggplot_histogram(data, variable, breaks = breaks_r()$brks)
        plot_histogram(data, variable, breaks = breaks_r()$brks)
      })


      data_cutted_r <- reactive({
        data <- req(data_r())
        variable <- req(input$variable)
        data[[paste0(variable, "_cut")]] <- cut(
          x = data[[variable]],
          breaks = breaks_r()$brks,
          include.lowest = input$include_lowest,
          right = input$right
        )
        code <- call2(
          "mutate",
          !!!set_names(
            list(
              expr(cut(
                !!!syms(list(x = variable)),
                !!!list(breaks = breaks_r()$brks, include.lowest = input$include_lowest, right = input$right)
              ))
            ),
            paste0(variable, "_cut")
          )
        )
        attr(data, "code") <- Reduce(
          f = function(x, y) expr(!!x %>% !!y),
          x = c(attr(data, "code"),  code)
        )
        data
      })

      output$count <- renderDatagrid2({
        data <- req(data_cutted_r())
        variable <- req(input$variable)
        count_data <- as.data.frame(
          table(
            breaks = data[[paste0(variable, "_cut")]]
          ),
          responseName = "count"
        )
        gridTheme <- getOption("datagrid.theme")
        if (length(gridTheme) < 1) {
          apply_grid_theme()
        }
        on.exit(toastui::reset_grid_theme())
        grid <- datagrid(
          data = count_data,
          colwidths = "guess",
          theme = "default",
          bodyHeight = "auto"
        )
        grid <- toastui::grid_columns(grid, className = "font-monospace")
        grid_colorbar(
          grid,
          column = "count",
          label_outside = TRUE,
          label_width = "40px",
          bar_bg = "#112466",
          from = c(0, max(count_data$count) + 1)
        )
      })

      data_returned_r <- observeEvent(input$create, {
        rv$data <- data_cutted_r()
      })
      return(reactive(rv$data))
    }
  )
}



#' @inheritParams shiny::modalDialog
#' @export
#'
#' @importFrom shiny showModal modalDialog textInput
#' @importFrom htmltools tagList
#'
#' @rdname cut-variable
modal_cut_variable <- function(id,
                               title = i18n("Convert Numeric to Factor"),
                               easyClose = TRUE,
                               size = "l",
                               footer = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(title, button_close_modal()),
    cut_variable_ui(id),
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = genId())
    ),
    easyClose = easyClose,
    size = size,
    footer = footer
  ))
}


#' @inheritParams shinyWidgets::WinBox
#' @export
#'
#' @importFrom shinyWidgets WinBox wbOptions wbControls
#' @importFrom htmltools tagList
#' @rdname cut-variable
winbox_cut_variable <- function(id,
                                title = i18n("Convert Numeric to Factor"),
                                options = shinyWidgets::wbOptions(),
                                controls = shinyWidgets::wbControls()) {
  ns <- NS(id)
  WinBox(
    title = title,
    ui = tagList(
      cut_variable_ui(id),
      tags$div(
        style = "display: none;",
        textInput(inputId = ns("hidden"), label = NULL, value = genId())
      )
    ),
    options = modifyList(
      shinyWidgets::wbOptions(height = "750px", modal = TRUE),
      options
    ),
    controls = controls,
    auto_height = FALSE
  )
}


#' @importFrom graphics abline axis hist par plot.new plot.window
plot_histogram <- function(data, column, bins = 30, breaks = NULL) {
  x <- data[[column]]
  op <- par(mar = rep(1.5, 4)); on.exit(par(op))
  plot.new()
  plot.window(xlim = range(pretty(x)), ylim =  range(pretty(hist(x, breaks = bins, plot = FALSE)$counts)))
  abline(v = pretty(x), col = "#D8D8D8")
  abline(h = pretty(hist(x, breaks = bins, plot = FALSE)$counts), col = "#D8D8D8")
  hist(x, breaks = bins, xlim = range(pretty(x)), xaxs = "i", yaxs = "i", col = "#112466", add = TRUE)
  axis(side = 1, at = pretty(x), pos = 0)
  axis(side = 2, at = pretty(hist(x, breaks = bins, plot = FALSE)$counts), pos = min(pretty(x)))
  abline(v = breaks, col = "#FFFFFF", lty = 1, lwd = 1.5)
  abline(v = breaks, col = "#2E2E2E", lty = 2, lwd = 1.5)
}


