
edit_data_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # Add a row --
    uiOutput(outputId = ns("add_button")),

    # Table --
    reactableOutput(outputId = ns("table"))
  )
}

edit_data_server <- function(id,
                             data_r = reactive(NULL), # reactive function with a data.frame
                             add = TRUE, # if true, allows you to add a row in the table via a button at the top right
                             update = TRUE, # if true, allows you to modify a row of the table via a button located in the table on the row you want to edit
                             delete = TRUE # if true, allows a row to be deleted from the table via a button in the table
) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      data_rv <- reactiveValues(data = NULL)

      # Data data_r() with added columns ".datamods_edit_update" et ".datamods_edit_delete" ---
      observeEvent(data_r(), {
        data <- data_r()
        data <- as.data.table(data)
        data <- data[, .datamods_edit_update := 1:nrow(data)]
        data <- data[, .datamods_edit_delete := 1:nrow(data)]
        data_rv$data <- data
      })

      # Table ---
      output$table <- renderReactable({
        req(data_r())
        data <- data_rv$data
        table(data = data,
              updateInputId = if (isTRUE(update)) ns("update"),
              deleteInputId = if (isTRUE(delete)) ns("delete"))
      })

      # Add a row ---
      output$add_button <- renderUI({
        if (isTRUE(add)) {
          actionButton(
            inputId = ns("add"),
            label = tagList(ph("plus"), "Add a row"),
            class = "btn-outline-primary float-end"
          )
        }
      })

      observeEvent(input$add, {
        req(data_r())
        input_window(id_validate = "add_row",
                     datas = data_rv$data)
      })

      observeEvent(input$add_row, {
        req(data_r())
        data <- data_rv$data
        data <- as.data.table(data)
        removeModal()
        list_inputs <- reactiveValuesToList(input)
        results_add <- try({
          results_inputs <- lapply(
            X = seq_len(ncol(data)),
            FUN = function(i) {
              inputs <- list()
              input_name <- names(data)[i]
              inputs[[i]] <- list_inputs[[input_name]]
            }
          )
          results_inputs[[ncol(data) - 1]] <- max(data$.datamods_edit_update) + 1
          results_inputs[[ncol(data)]] <- max(data$.datamods_edit_delete) + 1

          new <- data.frame(results_inputs)
          colnames(new) <- names(data)
          new <- data.table(new)
          data <- rbind(data, new, fill = TRUE)
          data_rv$data <- data
          #saveRDS(data, file = "datamods/data.rds") #changer nom
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
        req(data_r())
        data <- data_rv$data
        data <- as.data.table(data)
        row <- data[.datamods_edit_update == input$update]
        input_window(
          .data = row,
          title = "Update row",
          id_validate = "update_row",
          datas = data
        )
      })

      observeEvent(input$update_row, {
        req(data_r())
        data <- data_rv$data
        data <- as.data.table(data)
        removeModal()
        list_inputs <- reactiveValuesToList(input)
        results_update <- try({
            results_inputs <- lapply(
              X = seq_len(ncol(data)),
              FUN = function(i) {
                inputs <- list()
                input_name <- names(data)[i]
                inputs[[i]] <- list_inputs[[input_name]]
              }
            )
            results_inputs[[ncol(data) - 1]] <- data[.datamods_edit_update == input$update, .datamods_edit_update]
            results_inputs[[ncol(data)]] <- data[.datamods_edit_delete == input$update, .datamods_edit_delete]

            modification <- data.frame(results_inputs)
            colnames(modification) <- names(data)
            modification <- data.table(modification)

            data <- rbind(data[.datamods_edit_update != input$update], modification, fill = TRUE)
            data <- data[order(.datamods_edit_update)]
            data_rv$data <- data
            #saveRDS(data, file = "datamods/data.rds") #changer nom
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
        row <- data[.datamods_edit_delete == input$delete]
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
        row <- data[.datamods_edit_delete == input$delete]
        results_delete <- try({
          data <- data[.datamods_edit_delete != input$delete]
          data <- data[order(.datamods_edit_update)]
          data_rv$data <- data
          #saveRDS(data, file = "datamods/data.rds") #changer nom
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

      return(reactive(data_rv$data))

    }
  )
}



# Fonctions ---------------------------------------------------------------

input_window <- function(.data = list(),
                          id_validate = "add_row",
                          title = "Add a row",
                          datas,
                          session = getDefaultReactiveDomain()) {
  ns <- session$ns
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
    line_input(.data = .data, datas, session = session),
    actionButton(
      inputId = ns(id_validate),
      label = "Validate the entry",
      class = "btn-outline-primary float-end"
    )
  ))
}


line_input <- function(.data = list(), datas, session = getDefaultReactiveDomain()) {

  ns <- session$ns

  tagList(
    fluidRow(
      style = css(fontSize = "smaller"),
      column(
        width = 6,
        lapply(
          X = seq_len(ncol(datas)),
          FUN = function(i) {
            variable_name <- names(datas)[i]
            variable <- datas[[i]]

            if (isTRUE((inherits(x = variable, what = "numeric")))) {
              numericInput(
                inputId = ns(variable_name),
                label = paste0(variable_name, " : "),
                value = .data$variable_name %||% 0,
                width = "100%"
              )
            } else if (isTRUE((inherits(x = variable, what = "factor")))) {
              virtualSelectInput(
                inputId = ns(variable_name),
                label = paste0(variable_name, " : "),
                choices = unique(variable),
                selected = .data$variable_name %||% unique(variable)[[1]],
                width = "100%",
                allowNewOption = TRUE
              )
            } else if (isTRUE((inherits(x = variable, what = "character")))) {
              textInput(
                inputId = ns(variable_name),
                label = paste0(variable_name, " : "),
                value = .data$variable_name %||% "",
                width = "100%"
              )
            } else if (isTRUE((inherits(x = variable, what = "logical")))) {
              prettyCheckbox(
                inputId = ns("variable_name"),
                label = paste0(variable_name, " : "),
                value = .data$variable_name %||% FALSE,
                icon = icon("check"),
                status = "primary",
                width = "100%"
              )
            } else if (isTRUE((inherits(x = variable, what = "Date")))) {
              dateInput(
                inputId = ns("variable_name"),
                label = paste0(variable_name, " : "),
                value = .data$variable_name %||% Sys.Date(),
                width = "100%"
              )
            } else {
              return(NULL)
            }
          }
        )
      )
    )
  )
}


table <- function(data, updateInputId = NULL, deleteInputId = NULL) {
  reactable(
    data = data,
    columns = list(
      .datamods_edit_update = col_def_update(updateInputId),
      .datamods_edit_delete = col_def_delete(deleteInputId)
      )
    )
}

col_def_update <- function(inputId) {
  if (is.null(inputId))
    return(reactable::colDef(show = FALSE))
  reactable::colDef(
    name = "Update",
    width = 82,
    sortable = FALSE,
    html = TRUE,
    filterable = FALSE,
    cell = function(value) {
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
  )
}

col_def_delete <- function(inputId) {
  if (is.null(inputId))
    return(reactable::colDef(show = FALSE))
  reactable::colDef(
    name = "Delete",
    width = 96,
    sortable = FALSE,
    html = TRUE,
    filterable = FALSE,
    cell = function(value) {
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
  )
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
