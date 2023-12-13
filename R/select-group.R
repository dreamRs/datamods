
#' @title Select Group Input Module
#'
#' @description Group of mutually dependent select menus for filtering `data.frame`'s columns (like in Excel).
#'
#' @param id Module's id.
#' @param params A list of parameters passed to each [shinyWidgets::virtualSelectInput()],
#'  you can use :
#'   * `inputId`: mandatory, must correspond to variable name.
#'   * `label`: Display label for the control.
#'   * `placeholder`: Text to show when no options selected.
#' @param label Character, global label on top of all labels.
#' @param btn_reset_label Character, reset button label. If `NULL` no button is added.
#' @param inline If `TRUE` (the default),
#'  select menus are horizontally positioned, otherwise vertically.
#' @param vs_args Arguments passed to all [shinyWidgets::virtualSelectInput()] created.
#'
#' @return A [shiny::reactive()] function containing data filtered with an attribute `inputs` containing a named list of selected inputs.
#' @export
#'
#' @name select-group
#'
#' @importFrom utils modifyList
#' @importFrom htmltools tagList tags css
#' @importFrom shiny NS actionLink icon singleton
#' @importFrom shinyWidgets virtualSelectInput
#'
#' @example examples/select-group-default.R
select_group_ui <- function(id,
                            params,
                            label = NULL,
                            btn_reset_label = "Reset filters",
                            inline = TRUE,
                            vs_args = list()) {

  ns <- NS(id)

  button_reset <- if (!is.null(btn_reset_label)) {
    actionLink(
      inputId = ns("reset_all"),
      label = tagList(
        phosphoricons::ph("x", title = btn_reset_label),
        btn_reset_label
      ),
      icon = NULL,
      style = "float: right;"
    )
  }
  label_tag <- if (!is.null(label))
    tags$b(label, class = "select-group-label")

  sel_tag <- lapply(
    X = seq_along(params),
    FUN = function(x) {
      input <- params[[x]]
      vs_args <- modifyList(
        x = vs_args,
        val = list(
          inputId = ns(input$inputId),
          label = input$label,
          placeholder = input$placeholder,
          choices = NULL,
          selected = NULL,
          multiple = ifelse(is.null(input$multiple), TRUE, input$multiple),
          width = "100%",
          showValueAsTags = TRUE,
          zIndex = 10,
          disableSelectAll = TRUE
        ),
        keep.null = TRUE
      )
      tags$div(
        class = "select-group-item",
        id = ns(paste0("container-", input$inputId)),
        do.call(shinyWidgets::virtualSelectInput, vs_args)
      )
    }
  )

  if (isTRUE(inline)) {
    sel_tag <- tags$div(
      class = "select-group-container",
      style = htmltools::css(
        display = "grid",
        gridTemplateColumns = sprintf("repeat(%s, 1fr)", length(params)),
        gridColumnGap = "5px"
      ),
      sel_tag
    )
  }

  tags$div(
    class = "select-group",
    label_tag,
    sel_tag,
    button_reset,
    html_dependency_datamods()
  )
}



#' @param data_r Either a [data.frame()] or a [shiny::reactive()]
#'  function returning a `data.frame` (do not use parentheses).
#' @param vars_r character, columns to use to create filters,
#'  must correspond to variables listed in `params`. Can be a
#'  [shiny::reactive()] function, but values must be included in the initial ones (in `params`).
#'
#' @export
#'
#' @rdname select-group
#' @importFrom shiny observeEvent observe reactiveValues reactive is.reactive isolate isTruthy
#' @importFrom shinyWidgets updateVirtualSelect
select_group_server <- function(id, data_r, vars_r) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      # Namespace
      ns <- session$ns
      hideUI(selector = paste0("#", ns("reset_all")))


      # data <- as.data.frame(data)
      rv <- reactiveValues(data = NULL, vars = NULL)
      observe({
        if (is.reactive(data_r)) {
          rv$data <- data_r()
        } else {
          rv$data <- as.data.frame(data_r)
        }
        if (is.reactive(vars_r)) {
          rv$vars <- vars_r()
        } else {
          rv$vars <- vars_r
        }
        for (var in names(rv$data)) {
          if (var %in% rv$vars) {
            showUI(id = paste0("container-", var))
          } else {
            hideUI(id = paste0("container-", var))
          }
        }
      })

      observe({
        lapply(
          X = rv$vars,
          FUN = function(x) {
            vals <- sort(unique(rv$data[[x]]))
            shinyWidgets::updateVirtualSelect(
              session = session,
              inputId = x,
              choices = vals,
              selected = isolate(input[[x]])
            )
          }
        )
      })

      observeEvent(input$reset_all, {
        lapply(
          X = rv$vars,
          FUN = function(x) {
            vals <- sort(unique(rv$data[[x]]))
            shinyWidgets::updateVirtualSelect(
              session = session,
              inputId = x,
              choices = vals
            )
          }
        )
      })


      observe({
        vars <- rv$vars
        lapply(
          X = vars,
          FUN = function(x) {

            ovars <- vars[vars != x]

            observeEvent(input[[x]], {

              data <- rv$data

              indicator <- lapply(
                X = vars,
                FUN = function(x) {
                  data[[x]] %inT% input[[x]]
                }
              )
              indicator <- Reduce(f = `&`, x = indicator)
              data <- data[indicator, ]

              if (all(indicator)) {
                hideUI(selector = paste0("#", ns("reset_all")))
              } else {
                showUI(selector = paste0("#", ns("reset_all")))
              }

              for (i in ovars) {
                if (!isTruthy(input[[i]])) {
                  shinyWidgets::updateVirtualSelect(
                    session = session,
                    inputId = i,
                    choices = sort(unique(data[[i]]))
                  )
                }
              }

              if (!isTruthy(input[[x]])) {
                shinyWidgets::updateVirtualSelect(
                  session = session,
                  inputId = x,
                  choices = sort(unique(data[[x]]))
                )
              }

            }, ignoreNULL = FALSE, ignoreInit = TRUE)

          }
        )
      })

      return(reactive({
        data <- rv$data
        vars <- rv$vars
        indicator <- lapply(
          X = vars,
          FUN = function(x) {
            data[[x]] %inT% input[[x]]
          }
        )
        indicator <- Reduce(f = `&`, x = indicator)
        data <- data[indicator, ]
        attr(data, "inputs") <- lapply(
          X = setNames(vars, vars),
          FUN = function(x) input[[x]]
        )
        return(data)
      }))

    }
  )
}





