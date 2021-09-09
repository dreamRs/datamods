## Function sample_rows()

#' @title Sample rows
#'
#' @description The `sample_rows` function returns the sample of a dataset from a number of rows chosen by the user.
#'
#' @param data `data.frame`
#' @param rows vecteur de type `numeric`
#'
#' @return the sample of a dataset in the form of `data.table`
#'
#' @noRd
#'
#' @importFrom data.table as.data.table .N
#'
#' @examples
#' sample_rows(iris, 25)
sample_rows <- function(data, rows) {
  as.data.table(data)[sample(x = .N, size = rows)]
}


## Function sample_percentage()

#' @title Sample percentage
#'
#' @description The `sample_percentage` function returns the sample of a dataset from a percentage chosen by the user.
#'
#' @param data `data.frame`
#' @param percentage vecteur de type `numeric`
#'
#' @return the sample of a dataset in the form of `data.table`
#'
#' @noRd
#'
#' @importFrom data.table as.data.table .N
#'
sample_percentage <- function(data, percentage) {
  as.data.table(data)[sample(x = .N, size = nrow(data) * (percentage/100))]
}


## Function sample_ui()

#' @title Shiny Sample Module
#'
#' @param id Module ID
#'
#' @export
#'
#' @name module-echantillon
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
      ns("choice"),
      label = "Display :",
      choices = c("Percentage", "Rows"),
      justified = TRUE
    ),

    conditionalPanel(
      condition = "input.choice == `Percentage`",
      ns = ns,
      sliderInput(
        ns("bins"),
        label = "Choose a percentage:",
        min = 0, max = 100, value = 0,
        post = " %"),
      uiOutput(ns("text1"))
    ),

    conditionalPanel(
      condition = "input.choice == `Rows`",
      ns = ns,
      sliderInput(
        ns('slider_rows'),
        label = 'Choose a number of rows:',
        min = 0, max = 10, value = 10),
      uiOutput(ns("text2")),
    ),
  )
}


## Function sample_server()

#' @param my_data `reactive` containing a `data.frame` to use in the module.
#'
#' @export
#'
#' @rdname module-echantillon
#'
#' @importFrom shiny moduleServer observeEvent updateSliderInput renderUI reactive
#' @importFrom htmltools tags h4
#'
sample_server<- function(id, my_data) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(my_data(), {
        updateSliderInput(
          session,
          inputId = "slider_rows",
          min = 0, max = nrow(my_data()), value = min(0, nrow(my_data())
          )
        )
      })

      output$text1 <- renderUI({
        value <- nrow(my_data()) * (input$bins/100)
        tags$h4(paste(input$bins, "%, that is", round(value), "rows."))
      })

      output$text2 <- renderUI({
        value <- input$slider_rows / nrow(my_data()) * 100
        h4(paste(input$slider_rows, "rows, that is", round(value, 1), "%."))
      })

      sample <- reactive({
        if (input$choice == "Percentage") {
          table_sample <- sample_percentage(my_data(), input$bins)
        } else {
          table_sample <- sample_rows(my_data(), input$slider_rows)
        }
        return(table_sample)
      })

      return(sample)
    }
  )
}





















