
#' Function edit_data_ui()
#'
#' @param id Module ID
#'
#' @export
#'
#' @name module-data
#'
#' @examples
edit_data_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # Download data --
    conditionalPanel(
      condition = "output.download_csv == true",
      ns = ns,
      downloadButton(
        outputId = ns("export_csv"),
        label = tagList(
          ph("download"),
          "Export data in csv format"
        ),
        class = NULL,
        icon = NULL,
        width = "100%"
      )
    ),
    conditionalPanel(
      condition = "output.download_excel == true",
      ns = ns,
      downloadButton(
        outputId = ns("export_excel"),
        label = tagList(
          ph("download"),
          "Export data in excel format"
        ),
        class = NULL,
        icon = NULL,
        width = "100%"
      )
    ),

    # Add a row --
    uiOutput(outputId = ns("add_button")),

    # Table --
    reactableOutput(outputId = ns("table"))
  )
}

#' Function edit_data_server()
#'
#' @param id Module ID
#' @param data_r data_r `reactive` function containing a `data.frame` to use in the module.
#' @param add boolean, if TRUE, allows you to add a row in the table via a button at the top right
#' @param update boolean, if TRUE, allows you to modify a row of the table via a button located in the table on the row you want to edit
#' @param delete boolean, if true, allows a row to be deleted from the table via a button in the table
#'
#' @return the initial `data.frame` with the modifications applied
#'
#' @name module-data
#'
#' @export
#'
#' @examples
edit_data_server <- function(id,
                             data_r = reactive(NULL), # reactive function with a data.frame
                             add = TRUE, # if true, allows you to add a row in the table via a button at the top right
                             update = TRUE, # if true, allows you to modify a row of the table via a button located in the table on the row you want to edit
                             delete = TRUE, # if true, allows a row to be deleted from the table via a button in the table
                             download_csv = TRUE, # if true, allows to export the table in csv format via a download button
                             download_excel = TRUE, # if true, allows to export the table in excel format via a download button
                             file_name_export = "data", # character that allows you to choose the export name of the downloaded file
                             var_r = NULL, # vector of characters which allows to choose the names of the editable columns
                             var_mandatory = NULL # vector of characters which allows to choose obligatory fields to fill
) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      data_rv <- reactiveValues(data = NULL, colnames = NULL, mandatory = NULL)
      #page_rv <- reactiveValues(page = NULL)

      # Data data_r() with added columns ".datamods_edit_update" et ".datamods_edit_delete" ---
      data_init_r <- eventReactive(data_r(), {
        data <- data_r()
        if (is.reactive(var_mandatory))
          var_mandatory <- var_mandatory()
        data <- as.data.table(data)
        data_rv$colnames <- copy(colnames(data))
        setnames(data, paste0("col_", seq_along(data)))
        data_rv$mandatory <- colnames(data)[which(data_rv$colnames %in% var_mandatory)]

        data[, .datamods_id := seq_len(.N)]

        if (isTRUE(update)) {
          data[, .datamods_edit_update := as.character(seq_len(.N))]
          data[, .datamods_edit_update := lapply(.datamods_edit_update, btn_update(ns("update")))]
        } else {
          data[, .datamods_edit_update := NA]
        }

        if (isTRUE(delete)) {
        data[, .datamods_edit_delete := as.character(seq_len(.N))]
        data[, .datamods_edit_delete := lapply(.datamods_edit_delete, btn_delete(ns("delete")))]
        } else {
          data[, .datamods_edit_delete := NA]
        }

        data_rv$data <- data
        return(data)
      })

      # Table ---
      output$table <- renderReactable({
        data <- req(data_init_r())
        table_display(
          data = data,
          colnames = data_rv$colnames
        )
      })


      # Add a row ---
      output$add_button <- renderUI({
        if (is.reactive(add)) {
          add <- add()
        }
        if (isTRUE(add)) {
          tagList(
            actionButton(
              inputId = ns("add"),
              label = tagList(ph("plus"), "Add a row"),
              class = "btn-outline-primary float-end"
            ),
            tags$div(class = "clearfix")
          )
        }
      })

      observeEvent(input$add, {
        req(data_r())
        edit_modal(
          id_validate = "add_row",
          data = data_rv$data,
          colnames = data_rv$colnames,
          var = var_r,
          var_mandatory = var_mandatory
        )
      })

      observeEvent(input$add_row, {
        req(data_r())
        data <- data_rv$data
        data <- as.data.table(data)

        for (var in data_rv$mandatory) {
          if (!isTruthy(input[[var]])) {
            shinybusy::report_failure(
              title = "Required field",
              text = "Please fill in the required fields",
              button = "Close"
            )
            return(NULL)
          }
        }

        removeModal()

        results_add <- try({
          results_inputs <- lapply(
            X = seq_along(data),
            FUN = function(i) {
              input[[colnames(data)[i]]] %||% NA
            }
          )
          id <- max(data$.datamods_id) + 1
          results_inputs[[ncol(data) - 2]] <- id
          results_inputs[[ncol(data) - 1]] <- if (update) list(btn_update(ns("update"))(id)) else NA
          results_inputs[[ncol(data)]] <- if (delete) list(btn_delete(ns("delete"))(id)) else NA
          #results_inputs[[ncol(data)]] <- list(btn_delete(ns("delete"))(id))

          new <- as.data.table(results_inputs)
          setnames(new, colnames(data))

          # browser()

          data <- rbind(data, new, fill = TRUE)
          data_rv$data <- data
          page <- getReactableState(outputId = "table", name = "page")
          updateReactable("table", data = data, page = page)
        })
        if (inherits(results_add, "try-error")) {
          shinybusy::report_failure(
            title = "Error",
            text = "Unable to add the row, contact the platform administrator",
            button = "Close"
          )
        } else {
          shinybusy::report_success(
            title = "Registered",
            text = "Row has been saved",
            button = "Close"
          )
        }
      })


      # Update a row ---
      observeEvent(input$update, {
        data <- data_rv$data
        data <- as.data.table(data)
        row <- data[.datamods_id == input$update]
        edit_modal(
          default = row,
          title = "Update row",
          id_validate = "update_row",
          data = data,
          colnames = data_rv$colnames,
          var = var_r,
          var_mandatory = var_mandatory
        )
      })

      observeEvent(input$update_row, {
        req(data_r())
        data <- data_rv$data
        data <- as.data.table(data)
        #removeModal()
        #list_inputs <- reactiveValuesToList(input)

        for (var in data_rv$mandatory) {
          if (!isTruthy(input[[var]])) {
            shinybusy::report_failure(
              title = "Required field",
              text = "Please fill in the required fields",
              button = "Close"
            )
            return(NULL)
          }
        }

        removeModal()

        results_update <- try({
          results_inputs <- lapply(
            X = seq_along(data),
            FUN = function(i) {
              input[[colnames(data)[i]]] %||% NA
            }
          )

          id <- input$update
          results_inputs[[ncol(data) - 2]] <- id
          results_inputs[[ncol(data) - 1]] <- if (update) list(btn_update(ns("update"))(id)) else NA
          results_inputs[[ncol(data)]] <- if (delete) list(btn_delete(ns("delete"))(id)) else NA

          modification <- as.data.table(results_inputs)
          setnames(modification, colnames(data))

          data <- rbind(data[.datamods_id != input$update], modification, fill = TRUE)
          data <- data[order(.datamods_id)]
          data_rv$data <- data
          page <- getReactableState(outputId = "table", name = "page")
          updateReactable("table", data = data, page = page)
        })
        if (inherits(results_update, "try-error")) {
          shinybusy::report_failure(
            title = "Error",
            text = "Unable to modify the item, contact the platform administrator",
            button = "Close"
          )
        } else {
          shinybusy::report_success(
            title = "Registered",
            text = "Item has been modified",
            button = "Close"
          )
        }
      })


      # Delete a row ---
      observeEvent(input$delete, {
        req(data_r())
        data <- data_rv$data
        data <- as.data.table(data)
        row <- data[.datamods_id == input$delete]
        removeModal()
        showModal(confirmation_window(
          inputId = ns("confirmation_delete_row"),
          title = "Delete",
          "Do you want to delete the selected row ?"
        ))
      })
      observeEvent(input$confirmation_delete_row_yes, {
        req(data_r())
        data <- data_rv$data
        data <- as.data.table(data)
        row <- data[.datamods_id == input$delete]
        results_delete <- try({
          data <- data[.datamods_id != input$delete]
          data <- data[order(.datamods_id)]
          data_rv$data <- data
          page <- getReactableState(outputId = "table", name = "page")
          updateReactable("table", data = data, page = page)
        })
        if (inherits(results_delete, "try-error")) {
          shinybusy::report_failure(
            title = "Error",
            text = "Unable to delete the row, contact platform administrator",
            button = "Close"
          )
        } else {
          shinybusy::report_success(
            title = "Registered",
            text = "The row has been deleted",
            button = "Close"
          )
        }
        removeModal()
      })
      observeEvent(input$confirmation_delete_row_no, {
        shinybusy::report_info(
          title = "Information",
          text = "Row was not deleted",
          button = "Close"
        )
        removeModal()
      })


      # Download data ---
      ## Csv
      output[["download_csv"]] <- reactive({
        return(download_csv)
      })
      outputOptions(output, "download_csv", suspendWhenHidden = FALSE)

      output$export_csv <- downloadHandler(
        filename = function() {
          file_name <- file_name_export
          paste0(file_name, ".csv")
        },
        content = function(file) {
          req(data_r())
          data <- data_rv$data
          write.csv(
            x = data,
            file = file
          )
        }
      )
      ## Excel
      output[["download_excel"]] <- reactive({
        return(download_excel)
      })
      outputOptions(output, "download_excel", suspendWhenHidden = FALSE)

      output$export_excel <- downloadHandler(
        filename = function() {
          file_name <- file_name_export
          paste0(file_name, ".xlsx")
        },
        content = function(file) {
          req(data_r())
          data <- data_rv$data
          write_xlsx(
            x = list(data = req(data)),
            path = file
          )
        }
      )

      return(reactive(data_rv$data))

    }
  )
}



# Fonctions ---------------------------------------------------------------

#' Function edit_modal()
#'
#' @param default row on which to operate a modification or a deletion, otherwise empty list for an addition
#' @param id_validate inputId of the actionButton()
#' @param title title of the modalDialog()
#' @param data `data.frame` to use
#' @param session The `session` object passed to function given to shinyServer
#'
#' @return a modal input window with a validation button
#' @export
#'
#' @examples
edit_modal <- function(default = list(),
                       id_validate = "add_row",
                       title = "Add a row",
                       data,
                       colnames = names(data),
                       var,
                       var_mandatory,
                       session = getDefaultReactiveDomain()) {
  ns <- session$ns

  if (is.null(var)) {
    data <- data
  } else {
    data <- data[, ..var]
  }

  showModal(modalDialog(
    title = tagList(
      title,
      tags$button(
        phosphoricons::ph("x", title = "Close", height = "2em"),
        class = "btn btn-link",
        style = css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
        `data-bs-dismiss` = "modal",
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
      session = session
    ),
    actionButton(
      inputId = ns(id_validate),
      label = "Validate the entry",
      class = "btn-outline-primary float-end"
    )
  ))
}


edit_input_form <- function(default = list(), data, colnames, var_mandatory, session = getDefaultReactiveDomain()) {

  ns <- session$ns

  tagList(
    lapply(
      X = seq_len(ncol(data)),
      FUN = function(i) {
        variable_id <- colnames(data)[i]
        variable_name <- colnames[i]
        variable <- data[[i]]

        if (variable_name %in% var_mandatory) {
          label <- tags$p(variable_name, tags$span(HTML("&#42;"), class = "asterisk", style = "color: red;"), " : ")
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
            autoSelectFirstOption = FALSE
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
          dateInput(
            inputId = ns(variable_id),
            label = label,
            value = default[[variable_id]] %||% Sys.Date(),
            width = "100%"
          )
        } else {
          return(NULL)
        }
      }
    )
  )
}


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
  # if (hasName(data, ".datamods_edit_delete"))
  #   cols$.datamods_edit_delete = col_def_delete()

  cols$.datamods_id <- colDef(show = FALSE)
  reactable(
    data = data,
    columns = cols
  )
}

col_def_update <- function() {
  reactable::colDef(
    name = "Update",
    width = 82,
    sortable = FALSE,
    html = TRUE,
    filterable = FALSE
    # cell = btn_update(inputId)
  )
}

btn_update <- function(inputId) {
  function(value) {
    tags$button(
      class = "btn btn-outline-primary rounded-circle",
      style = htmltools::css(
        height = "40px",
        width = "40px",
        padding = 0
      ),
      onClick = sprintf(
        "Shiny.setInputValue(\'%s\', %s,  {priority: \'event\'})",
        inputId, value
      ),
      title = "Click to edit",
      ph("pencil-simple-line", height = "1.2em")
    ) %>%
      htmltools::doRenderTags()
  }
}

col_def_delete <- function() {
  reactable::colDef(
    name = "Delete",
    width = 96,
    sortable = FALSE,
    html = TRUE,
    filterable = FALSE
    # cell = btn_delete(inputId)
  )
}

btn_delete <- function(inputId) {
  function(value) {
    tags$button(
      class = "btn btn-outline-danger rounded-circle",
      style = htmltools::css(
        height = "40px",
        width = "40px",
        padding = 0
      ),
      onClick = sprintf(
        "Shiny.setInputValue(\'%s\', %s,  {priority: \'event\'})",
        inputId, value
      ),
      title = "Click to delete",
      ph("x", height = "1.2em")
    ) %>%
      htmltools::doRenderTags()
  }
}


confirmation_window <- function(inputId, ..., title = NULL) {
  modalDialog(
    title = tagList(
      tags$button(
        phosphoricons::ph("x", title = "Close", height = "2em"),
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
        "Cancel",
        class = "btn btn-outline-secondary",
        `data-bs-dismiss` = "modal"
      ),
      actionButton(
        inputId = paste0(inputId, "_no"),
        label = "No",
        class = "btn-outline-danger",
        `data-bs-dismiss` = "modal"
      ),
      actionButton(
        inputId = paste0(inputId, "_yes"),
        label = "Yes",
        class = "btn-outline-primary"
      )
    )
  )
}
