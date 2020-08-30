
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
    tags$h2("Import Google Spreadsheet"),
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
    tags$div(
      id = ns("validate-button"),
      style = "margin-top: 20px;",
      actionButton(
        inputId = ns("validate"),
        label = "Import data",
        icon = icon("arrow-circle-right"),
        width = "100%",
        disabled = "disabled",
        class = "btn-primary"
      )
    )
  )
}

#' @param default_data Default \code{data.frame} to use.
#' @param update_data When to update selected data:
#'  \code{"button"} (when user click on button) or
#'  \code{"always"} (each time user select a dataset in the list).
#'
#' @export
#'
#' @importFrom shiny callModule
#'
#' @rdname import-googlesheets

import_googlesheets_server <- function(id,
                                       default_data = NULL,
                                       update_data = c("button", "always")) {
  callModule(
    module = import_googlesheets,
    id = id,
    default_data = default_data,
    update_data = update_data
  )
}


#' @importFrom googlesheets4 range_read gs4_auth gs4_deauth gs4_has_token
#' @importFrom shiny reactiveValues observeEvent removeUI reactive
#' @importFrom htmltools tags tagList
import_googlesheets <- function(input, output, session,
                                default_data = NULL,
                                update_data = c("button", "always")) {

  ns <- session$ns
  update_data <- match.arg(update_data)
  imported_data <- reactiveValues(data = default_data)
  temporary_data <- reactiveValues(data = default_data)

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


  if (identical(update_data, "always")) {
    removeUI(selector = paste0("#", ns("validate-button")))
  }


  observeEvent(input$link, {

    if (isFALSE(googlesheets4::gs4_has_token())) {
      googlesheets4::gs4_deauth()
    }

    imported <- try(googlesheets4::range_read(input$link), silent = TRUE)

    if (inherits(imported, "try-error") || NROW(imported) < 1) {

      toggle_widget(inputId = ns("validate"), enable = FALSE)
      insert_alert(
        selector = ns("import"),
        status = "danger",
        tags$b(icon("exclamation-triangle"), "Ooops"), "Something went wrong..."
      )

    } else {

      toggle_widget(inputId = ns("validate"), enable = TRUE)

      if (identical(update_data, "button")) {
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

      temporary_data$data <- imported
    }
  }, ignoreInit = TRUE)

  observeEvent(input$see_data, {
    show_data(temporary_data$data)
  })

  observeEvent(input$validate, {
    imported_data$data <- temporary_data$data
  })

  if (identical(update_data, "button")) {
    return(list(
      data = reactive(imported_data$data)
    ))
  } else {
    return(list(
      data = reactive(temporary_data$data)
    ))
  }
}
