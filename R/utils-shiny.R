
#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
html_dependency_datamods <- function() {
  htmlDependency(
    name = "datamods",
    version = packageVersion("datamods"),
    src = list(href = "datamods", file = "assets"),
    package = "datamods",
    script = "js/datamods.js",
    stylesheet = "css/datamods.css"
  )
}


#' Enable or disable a widget from server
#'
#' @param inputId Widget's inputId.
#' @param enable Enable or disable the input.
#' @param session Shiny session.
#'
#' @noRd
toggle_widget <- function(inputId,
                        enable = TRUE,
                        session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "datamods-toggleWidget",
    message = list(id = session$ns(inputId), enable = enable)
  )
}


#' Insert an alert into a placeholder in UI
#'
#' @param selector Id for alert, the placeholder maust have \code{"-placeholder"} suffix.
#' @param ... Arguments passed to \code{shinyWidgets::alert}.
#'
#' @return No value.
#' @noRd
#'
#' @importFrom shiny removeUI insertUI
#' @importFrom shinyWidgets alert
#'
insert_alert <- function(selector, ...) {
  removeUI(selector = paste0("#", selector, "-result"))
  insertUI(
    selector = paste0("#", selector, "-placeholder"),
    ui = alert(
      id = paste0(selector, "-result"),
      ...
    )
  )
}





showUI <- function(selector, inline = FALSE, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "datamods-showUI",
    message = list(selector = selector, inline = inline)
  )
}

hideUI <- function(selector, inline = FALSE, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "datamods-hideUI",
    message = list(selector = selector, inline = inline)
  )
}


#' @importFrom htmltools tagList tags
#' @importFrom shiny icon getDefaultReactiveDomain
make_success_alert <- function(data,
                               trigger_return,
                               btn_show_data,
                               session = shiny::getDefaultReactiveDomain()) {
  if (identical(trigger_return, "button")) {
    success_message <- tagList(
      tags$b(icon("check"), "Data ready to be imported!"),
      sprintf(
        "data has %s obs. of %s variables",
        nrow(data), ncol(data)
      )
    )
  } else {
    success_message <- tagList(
      tags$b(icon("check"), "Data successfully imported!"),
      sprintf(
        "data has %s obs. of %s variables",
        nrow(data), ncol(data)
      )
    )
  }
  if (isTRUE(btn_show_data)) {
    success_message <- tagList(
      success_message,
      tags$br(),
      actionLink(
        inputId = session$ns("see_data"),
        label = "click to see data",
        icon = icon("hand-o-right")
      )
    )
  }
  return(success_message)
}
