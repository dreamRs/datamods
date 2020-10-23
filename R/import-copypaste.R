
#' @title Import data with copy & paste
#'
#' @description Let the user copy data from Excel or text file then paste it into a text area to import it.
#'
#' @param id Module's ID.
#'
#' @return
#'  * UI: HTML tags that can be included in shiny's UI
#'  * Server: a \code{list} with one slot:
#'    + **data**: a \code{reactive} function returning the selected \code{data.frame}.
#'
#' @export
#'
#' @name import-copypaste
#'
#' @importFrom shiny NS icon textAreaInput actionButton
#' @importFrom htmltools tags tagAppendAttributes
#'
#' @example examples/copypaste.R
import_copypaste_ui <- function(id) {

  ns <- NS(id)

  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    tags$h3("Copy & paste data"),
    tagAppendAttributes(
      textAreaInput(
        inputId = ns("data_pasted"),
        label = "Paste data here:",
        height = "300px",
        width = "100%",
        resize = "none"
      ),
      class = "shiny-input-container-inline"
    ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b("Nothing pasted yet!"),
        "Please copy and paste some data in the dialog box above.",
        dismissible = TRUE
      )
    ),
    uiOutput(
      outputId = ns("container_valid_btn"),
      style = "margin-top: 20px;"
    )
  )
}


#' @param trigger_return When to update selected data:
#'  \code{"button"} (when user click on button) or
#'  \code{"change"} (each time user select a dataset in the list).
#' @param return_class Class of returned data: \code{data.frame}, \code{data.table} or \code{tbl_df} (tibble).
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom data.table fread
#' @importFrom shiny reactiveValues observeEvent removeUI reactive
#' @importFrom htmltools tags tagList
#'
#' @rdname import-copypaste
import_copypaste_server <- function(id,
                                    trigger_return = c("button", "change"),
                                    return_class = c("data.frame", "data.table", "tbl_df")) {

  trigger_return <- match.arg(trigger_return)

  module <- function(input, output, session) {

    ns <- session$ns
    imported_rv <- reactiveValues(data = NULL)
    temporary_rv <- reactiveValues(data = NULL)

    output$container_valid_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        actionButton(
          inputId = ns("validate"),
          label = "Import data",
          icon = icon("arrow-circle-right"),
          width = "100%",
          disabled = "disabled",
          class = "btn-primary"
        )
      }
    })

    observeEvent(input$data_pasted, {
      req(input$data_pasted)
      imported <- try(data.table::fread(text = input$data_pasted), silent = TRUE)

      if (inherits(imported, "try-error") || NROW(imported) < 1) {

        toggle_widget(inputId = "validate", enable = FALSE)

        insert_alert(
          selector = ns("import"),
          status = "danger",
          tags$b(icon("exclamation-triangle"), "Ooops"), "Something went wrong..."
        )

      } else {

        toggle_widget(inputId = "validate", enable = TRUE)

        if (identical(trigger_return, "button")) {
          success_message <- tagList(
            tags$b(icon("check"), "Data ready to be imported!"),
            sprintf(
              "%s obs. of %s variables imported",
              nrow(imported), ncol(imported)
            )
          )
        } else {
          success_message <- tagList(
            tags$b(icon("check"), "Data successfully imported!"),
            sprintf(
              "%s obs. of %s variables imported",
              nrow(imported), ncol(imported)
            )
          )
        }
        success_message <- tagList(
          success_message,
          tags$br(),
          actionLink(
            inputId = ns("see_data"),
            label = "click to see data",
            icon = icon("hand-o-right")
          )
        )

        insert_alert(
          selector = ns("import"),
          status = "success",
          success_message
        )

        temporary_rv$data <- imported
      }
    }, ignoreInit = TRUE)

    observeEvent(input$see_data, {
      show_data(temporary_rv$data)
    })

    observeEvent(input$validate, {
      imported_rv$data <- temporary_rv$data
    })


    if (identical(trigger_return, "button")) {
      return(list(
        data = reactive(as_out(imported_rv$data, return_class))
      ))
    } else {
      return(list(
        data = reactive(as_out(temporary_rv$data, return_class))
      ))
    }
  }

  moduleServer(
    id = id,
    module = module
  )
}



