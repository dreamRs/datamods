
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


enable_tab <- function(id, value, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "datamods-enableTab",
    message = list(id = session$ns(id), value = value)
  )
}

disable_tab <- function(id, value, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "datamods-disableTab",
    message = list(id = session$ns(id), value = value)
  )
}

#' @importFrom htmltools doRenderTags
update_tab_label <- function(id, value, label, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "datamods-updateTabLabel",
    message = list(id = session$ns(id), value = value, label = doRenderTags(label))
  )
}


#' @importFrom htmltools tagList tags
#' @importFrom shiny icon getDefaultReactiveDomain
make_success_alert <- function(data,
                               trigger_return,
                               btn_show_data,
                               extra = NULL,
                               session = shiny::getDefaultReactiveDomain()) {
  if (identical(trigger_return, "button")) {
    success_message <- tagList(
      tags$b(icon("check"), i18n("Data ready to be imported!")),
      sprintf(
        i18n("data has %s obs. of %s variables."),
        nrow(data), ncol(data)
      ),
      extra
    )
  } else {
    success_message <- tagList(
      tags$b(icon("check"), i18n("Data successfully imported!")),
      sprintf(
        i18n("data has %s obs. of %s variables."),
        nrow(data), ncol(data)
      ),
      extra
    )
  }
  if (isTRUE(btn_show_data)) {
    success_message <- tagList(
      success_message,
      tags$br(),
      actionLink(
        inputId = session$ns("see_data"),
        label = i18n("click to see data"),
        icon = icon("hand-o-right")
      )
    )
  }
  return(success_message)
}

insert_error <- function(selector = "import",
                         session = shiny::getDefaultReactiveDomain()) {
  insert_alert(
    selector = session$ns(selector),
    status = "danger",
    tags$b(icon("exclamation-triangle"), i18n("Ooops")),
    i18n("Something went wrong...")
  )
}


#' @importFrom htmltools tagList tags doRenderTags
help_popup <- function(text) {
  tagList(
    tags$span(
      icon("question"),
      `data-toggle` = "popover",
      `data-trigger` = "focus",
      title = i18n("Help"),
      `data-html` = "true",
      `data-content` = htmltools::doRenderTags(text),
      tabindex = "0",
      role = "button"
    ),
    tags$script(
      "$(function () { $(\'[data-toggle=\"popover\"]\').popover({container: 'body'}); })"
    )
  )
}

#' @importFrom shiny actionButton icon getDefaultReactiveDomain
button_import <- function(session = shiny::getDefaultReactiveDomain()) {
  actionButton(
    inputId = session$ns("confirm"),
    label = i18n("Import data"),
    icon = icon("arrow-circle-right"),
    width = "100%",
    disabled = "disabled",
    class = "btn-primary",
    `aria-label` = i18n("Import data")
  )
}
