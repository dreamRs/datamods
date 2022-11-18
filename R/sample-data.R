## Function sample_n()

#' @title Sample rows
#'
#' @description The `sample_n` function returns the sample of a dataset from a number of rows chosen by the user.
#'
#' @param data `data.frame`
#' @param n vector of type `numeric`
#'
#' @return the sample of a dataset in the form of `data.table`
#'
#' @noRd
#'
#' @importFrom data.table as.data.table .N
#'
#' @examples
#' sample_n(iris, 25)
sample_n <- function(data, n) {
  as.data.table(data)[sample(x = .N, size = n)]
}


## Function sample_prop()

#' @title Sample percentage
#'
#' @description The `sample_prop` function returns the sample of a dataset from a percentage chosen by the user.
#'
#' @param data `data.frame`
#' @param percentage vector of type `numeric`
#'
#' @return the sample of a dataset in the form of `data.table`
#'
#' @noRd
#'
#' @importFrom data.table as.data.table .N
#'
sample_prop <- function(data, prop) {
  as.data.table(data)[sample(x = .N, size = nrow(data) * (prop/100))]
}


## Function sample_ui()

#' @title Shiny module to interactively sample a `data.frame`
#'
#' @description Allow to take a sample of `data.frame` for a given number or proportion of rows to keep.
#'
#' @param id Module id. See [shiny::moduleServer()].
#'
#' @return
#' * UI: HTML tags that can be included in shiny's UI
#' * Server: a `reactive` fgunction with the sampled data.
#'
#' @export
#'
#' @name module-sample
#'
#' @importFrom htmltools tagList
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shiny NS conditionalPanel sliderInput uiOutput
#'
#' @example examples/sample.R
sample_ui <- function(id) {
  ns <- NS(id)

  tagList(
    radioGroupButtons(
      inputId = ns("choice"),
      label = "Sample data by :",
      choices = c("number of rows", "proportion of rows"),
      justified = FALSE,
      size = "xs"
    ),

    conditionalPanel(
      condition = "input.choice == `proportion of rows`",
      ns = ns,
      sliderInput(
        inputId = ns("proportion_rows"),
        label = "Choose a percentage :",
        min = 0, max = 100, value = 100,
        post = " %"
        ),
      uiOutput(outputId = ns("feedback_proportion_rows"))
    ),

    conditionalPanel(
      condition = "input.choice == `number of rows`",
      ns = ns,
      sliderInput(
        inputId = ns("number_rows"),
        label = "Choose a number of rows :",
        min = 0, max = 10, value = 10
        ),
      uiOutput(outputId = ns("feedback_number_rows"))
    )
  )
}


## Function sample_server()

#' @param data_r `reactive` containing a `data.frame` to use in the module.
#'
#' @export
#'
#' @rdname module-sample
#'
#' @importFrom shiny moduleServer observeEvent updateSliderInput renderUI reactive
#' @importFrom htmltools tags div
#'
sample_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(data_r(), {
        req(data_r())
        updateSliderInput(
          session,
          inputId = "number_rows",
          min = 0, max = nrow(data_r()), value = nrow(data_r())
        )
      })

      output$feedback_proportion_rows <- renderUI({
        req(data_r())
        value <- nrow(data_r()) * (input$proportion_rows/100)
        tags$div(paste(input$proportion_rows, "% of the total, i.e.", round(value), "rows"))
      })

      output$feedback_number_rows <- renderUI({
        req(data_r())
        value <- input$number_rows / nrow(data_r()) * 100
        tags$div(paste(input$number_rows, "lines, i.e.", round(value, 1), "% of the total"))
      })

      sample_r <- reactive({
        req(data_r())
        if (input$choice == "proportion of rows") {
          table_sample <- sample_prop(data = data_r(), prop = input$proportion_rows)
        } else {
          table_sample <- sample_n(data = data_r(), n = input$number_rows)
        }
        return(table_sample)
      })

      return(sample_r)
    }
  )
}


