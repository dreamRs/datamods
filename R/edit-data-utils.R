

#' @title Edit modal
#'
#' @description The `edit_modal` function generates a modal window with the variables to edit
#'
#' @return a modal window with the variables to edit
#'
#' @param default row on which to operate a modification or a deletion, otherwise empty list for an addition
#' @param id_validate inputId of the actionButton()
#' @param title title of the modalDialog()
#' @param data `data.frame` to use
#' @param colnames `data.frame` column names
#' @param var_edit vector of `character` which allows to choose the editable columns
#' @param var_mandatory vector of `character` which allows to choose obligatory fields to fill
#' @param session The `session` object passed to function given to shinyServer
#'
#' @importFrom shiny showModal modalDialog actionButton
#' @importFrom phosphoricons ph
#' @importFrom htmltools tagList tags css
#'
#' @noRd
#'
edit_modal <- function(default = list(),
                       id_validate = "add_row",
                       title = i18n("Add a row"),
                       data,
                       colnames = names(data),
                       var_edit,
                       var_mandatory,
                       session = getDefaultReactiveDomain()) {
  ns <- session$ns

  if (identical(x = var_edit, y = character(0)) | identical(x = var_edit, y = NULL)) {
    data <- data
    position_var_edit <- seq_len(ncol(data))
  } else {
    data <- data[, ..var_edit]
    position_var_edit <- as.numeric(gsub("col_", "", var_edit))
  }

  showModal(modalDialog(
    title = tagList(
      title,
      tags$button(
        phosphoricons::ph("x", title = i18n("Close"), height = "2em"),
        class = "btn btn-link",
        style = css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
        `data-bs-dismiss` = "modal",
        `data-dismiss` = "modal",
        `aria-label` = "Close"
      )
    ),
    footer = NULL,
    size = "m",
    easyClose = TRUE,
    edit_input_form(
      default = default,
      data = data,
      colnames = colnames,
      var_mandatory = var_mandatory,
      position_var_edit =  position_var_edit,
      session = session
    ),
    actionButton(
      inputId = ns(id_validate),
      label = i18n("Validate the entry"),
      class = "btn-outline-primary float-end"
    )
  ))
}


#' @title Edit input form
#'
#' @description The `edit_input_form` function allows to correctly generate the variables to be edited in the modal window according to their respective class
#'
#' @param default default row on which to operate a modification or a deletion, otherwise empty list for an addition
#' @param data `data.frame` to use
#' @param colnames `data.frame` column names
#' @param var_mandatory vector of `character` which allows to choose obligatory fields to fill
#' @param position_var_edit position of editable columns in order to retrieve their name
#' @param session The `session` object passed to function given to shinyServer
#'
#' @importFrom shiny numericInput textInput
#' @importFrom shinyWidgets virtualSelectInput prettyCheckbox airDatepickerInput
#' @importFrom htmltools tagList tags
#'
#' @return different shiny widgets with edited columns according to their respective class
#' @noRd
#'
edit_input_form <- function(default = list(), data, colnames, var_mandatory, position_var_edit, session = getDefaultReactiveDomain()) {

  ns <- session$ns

  tagList(
    lapply(
      X = seq_len(ncol(data)),
      FUN = function(i) {
        variable_id <- colnames(data)[i]
        variable_name <- colnames[position_var_edit[i]]
        variable <- data[[i]]

        if (variable_name %in% var_mandatory) {
          label <- tagList(variable_name, tags$span(HTML("&#42;"), class = "asterisk", style = "color: red;"), " : ")
        } else {
          label <- paste0(variable_name, " : ")
        }

        if (isTRUE((inherits(x = variable, what = "numeric")))) {
          numericInput(
            inputId = ns(variable_id),
            label = label,
            value = default[[variable_id]] %||% NA_real_,
            width = "100%"
          )
        } else if (isTRUE((inherits(x = variable, what = "factor")))) {
          virtualSelectInput(
            inputId = ns(variable_id),
            label = label,
            choices = unique(variable),
            selected = default[[variable_id]] %||% "",
            width = "100%",
            allowNewOption = TRUE,
            autoSelectFirstOption = FALSE,
            placeholder = i18n("Select")
          )
        } else if (isTRUE((inherits(x = variable, what = "character")))) {
          textInput(
            inputId = ns(variable_id),
            label = label,
            value = default[[variable_id]] %||% "",
            width = "100%"
          )
        } else if (isTRUE((inherits(x = variable, what = "logical")))) {
          prettyCheckbox(
            inputId = ns(variable_id),
            label = label,
            value = default[[variable_id]] %||% FALSE,
            icon = icon("check"),
            status = "primary",
            width = "100%"
          )
        } else if (isTRUE((inherits(x = variable, what = "Date")))) {
          airDatepickerInput(
            inputId = ns(variable_id),
            label = label,
            value = default[[variable_id]] %||% Sys.Date(),
            inline = TRUE,
            width = "100%"
          )
        } else {
          return(NULL)
        }
      }
    )
  )
}


#' @title Table display
#'
#' @description The `table_display` function allows you to display the table in reactable format with columns to edit and delete rows
#'
#' @param data `data.frame` to use
#' @param colnames `data.frame` column names
#'
#' @return the `data.frame` in reactable format
#' @noRd
#'
#' @importFrom reactable reactable colDef
#'
table_display <- function(data, colnames = NULL) {
  cols <- list()
  for (i in seq_along(data)) {
    cols[[names(data)[i]]] <- colDef(name = colnames[i])
  }
  if (all(is.na(data$.datamods_edit_update))) {
    cols$.datamods_edit_update = colDef(show = FALSE)
  } else {
    cols$.datamods_edit_update = col_def_update()
  }

  if (all(is.na(data$.datamods_edit_delete))) {
    cols$.datamods_edit_delete = colDef(show = FALSE)
  } else {
    cols$.datamods_edit_delete = col_def_delete()
  }

  cols$.datamods_id <- colDef(show = FALSE)
  reactable(
    data = data,
    columns = cols
  )
}


#' @title The update column definition
#'
#' @return A column definition object that can be used to customize the update column in reactable().
#' @noRd
#'
#' @importFrom reactable colDef
#'
col_def_update <- function() {
  colDef(
    name = i18n("Update"),
    width = 82,
    sortable = FALSE,
    html = TRUE,
    filterable = FALSE
  )
}

#' The update button
#'
#' @param inputId ID
#'
#' @return the update button
#' @noRd
#'
#' @importFrom htmltools tags css doRenderTags
#' @importFrom phosphoricons ph
#'
btn_update <- function(inputId) {
  function(value) {
    htmltools::doRenderTags(
      tags$button(
        class = "btn btn-outline-primary rounded-circle",
        style = htmltools::css(
          height = "40px",
          width = "40px",
          padding = 0
        ),
        onClick = sprintf(
          "Shiny.setInputValue(\'%s\', %s,  {priority: \'event\'})",
          inputId,
          value
        ),
        title = i18n("Click to edit"),
        ph("pencil-simple-line", height = "1.2em")
      )
   )

  }
}


#' @title The delete column definition
#'
#' @return A column definition object that can be used to customize the delete column in reactable().
#' @noRd
#' @importFrom reactable colDef
#'
col_def_delete <- function() {
  reactable::colDef(
    name = i18n("Delete"),
    width = 96,
    sortable = FALSE,
    html = TRUE,
    filterable = FALSE
  )
}

#' The delete button
#'
#' @param inputId ID
#'
#' @return the delete button
#' @noRd
#'
#' @importFrom htmltools tags css doRenderTags
#' @importFrom phosphoricons ph
#'
btn_delete <- function(inputId) {
  function(value) {
    htmltools::doRenderTags(
      tags$button(
        class = "btn btn-outline-danger rounded-circle",
        style = htmltools::css(
          height = "40px",
          width = "40px",
          padding = 0
        ),
        onClick = sprintf(
          "Shiny.setInputValue(\'%s\', %s,  {priority: \'event\'})",
          inputId,
          value
        ),
        title = i18n("Click to delete"),
        ph("x", height = "1.2em")
      )
   )
  }
}


#' Confirmation window
#'
#' @param inputId ID
#' @param ... optional additional elements to add in the ui
#' @param title title of the confirmation window
#'
#' @return a confirmation window
#' @noRd
#'
#' @importFrom shiny modalDialog actionButton
#' @importFrom htmltools tagList tags css
#' @importFrom phosphoricons ph
#'
confirmation_window <- function(inputId, ..., title = NULL) {
  modalDialog(
    title = tagList(
      tags$button(
        phosphoricons::ph("x", title = i18n("Close"), height = "2em"),
        class = "btn btn-link",
        style = css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
        `data-bs-dismiss` = "modal",
        `aria-label` = "Fermer"
      ),
      title
    ),
    ...,
    size = "m",
    footer = tagList(
      tags$button(
        i18n("Cancel"),
        class = "btn btn-outline-secondary",
        `data-bs-dismiss` = "modal"
      ),
      actionButton(
        inputId = paste0(inputId, "_no"),
        label = i18n("No"),
        class = "btn-outline-danger",
        `data-bs-dismiss` = "modal"
      ),
      actionButton(
        inputId = paste0(inputId, "_yes"),
        label = i18n("Yes"),
        class = "btn-outline-primary"
      )
    )
  )
}


#' @importFrom shinybusy notify_failure notify_success notify_info notify_warning
notification_failure <- function(title, text) {
  shinybusy::notify_failure(
    title = title,
    text = text,
    position = "center-top",
    clickToClose = TRUE
  )
}
notification_warning <- function(title, text) {
  shinybusy::notify_warning(
    title = title,
    text = text,
    position = "center-top",
    clickToClose = TRUE
  )
}
notification_success <- function(title, text) {
  shinybusy::notify_success(
    title = title,
    text = text,
    position = "center-top",
    clickToClose = TRUE
  )
}
notification_info <- function(title, text) {
  shinybusy::notify_info(
    title = title,
    text = text,
    position = "center-top",
    clickToClose = TRUE
  )
}
