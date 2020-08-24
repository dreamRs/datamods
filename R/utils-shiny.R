
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
    message = list(id = inputId, enable = enable)
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





