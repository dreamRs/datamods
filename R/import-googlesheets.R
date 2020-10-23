
#' @title Import data with a googlesheet link
#'
#' @description Let user paste link to a Google sheet then import the data.
#'
#' @param id Module's ID
#'
#' @return
#'  * UI: HTML tags that can be included in shiny's UI
#'  * Server: a \code{list} with one slot:
#'    + **data**: a \code{reactive} function returning the selected \code{data.frame}.
#'
#' @export
#' @name import-googlesheets
#'
#' @importFrom shiny NS actionLink
#' @importFrom shinyWidgets textInputIcon
#' @importFrom htmltools tags
#'
#' @example examples/googlesheets.R
import_googlesheets_ui <- function(id) {

  ns <- NS(id)

  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    tags$h3("Import Google Spreadsheet"),
    tags$p("If you have a shareable link, paste it directly in the field below"),
    tags$p(
      "Otherwise",
      actionLink(
        inputId = ns("sign_in"),
        label = "sign-in to Google",
      )
    ),
    tags$br(),
    tags$div(
      id = ns("signin-placeholder"),
      alert(
        id = ns("signin-result"),
        status = "info",
        tags$b("Status:"),
        "Not signed in.",
        dismissible = TRUE
      )
    ),
    shinyWidgets::textInputIcon(
      inputId = ns("link"),
      label = "Enter a URL to a Google Sheet:",
      icon = icon("link"),
      width = "100%"
    ),
    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b("Nothing pasted yet!"),
        "Please paste a valid googlesheets link in the dialog box above.",
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
#' @importFrom googlesheets4 range_read gs4_auth gs4_deauth gs4_has_token
#' @importFrom shiny reactiveValues observeEvent removeUI reactive
#' @importFrom htmltools tags tagList
#'
#' @rdname import-googlesheets

import_googlesheets_server <- function(id,
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

    options(gargle_oauth_cache = FALSE)

    observeEvent(input$sign_in, {
      googlesheets4::gs4_auth()

      if (googlesheets4::gs4_has_token()) {
        insert_alert(
          selector = ns("signin"),
          status = "success",
          tags$b("Status:"),
          "Signed in!"
        )
      } else {
        insert_alert(
          selector = ns("signin"),
          status = "info",
          tags$b("Status:"),
          "Not signed in."
        )
      }
    })


    observeEvent(input$trigger, {
      if (identical(trigger_return, "change")) {
        hideUI(selector = paste0("#", ns("validate-button")))
      }
    })


    observeEvent(input$link, {

      if (isFALSE(googlesheets4::gs4_has_token())) {
        googlesheets4::gs4_deauth()
      }

      imported <- try(googlesheets4::range_read(input$link), silent = TRUE)

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



