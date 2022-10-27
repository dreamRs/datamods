
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

    # Download data in Excel format --
    uiOutput(outputId = ns("download_excel")),

    # Download data in csv format --
    uiOutput(outputId = ns("download_csv")),

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
                             var_edit = NULL, # vector of characters which allows to choose the names of the editable columns
                             var_mandatory = NULL # vector of characters which allows to choose obligatory fields to fill
) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      data_rv <- reactiveValues(data = NULL, colnames = NULL, mandatory = NULL, edit = NULL)

      # Data data_r() with added columns ".datamods_edit_update" et ".datamods_edit_delete" ---
      data_init_r <- eventReactive(data_r(), {
        data <- data_r()
        if (is.reactive(var_mandatory))
          var_mandatory <- var_mandatory()
        if (is.reactive(var_edit))
          var_edit <- var_edit()
        data <- as.data.table(data)
        data_rv$colnames <- copy(colnames(data))
        setnames(data, paste0("col_", seq_along(data)))
        data_rv$mandatory <- colnames(data)[which(data_rv$colnames %in% var_mandatory)]
        data_rv$edit <- colnames(data)[which(data_rv$colnames %in% var_edit)]

        data[, .datamods_id := seq_len(.N)]

        if (is.reactive(update)) {
          update <- update()
        }

        if (isTRUE(update)) {
          data[, .datamods_edit_update := as.character(seq_len(.N))]
          data[, .datamods_edit_update := lapply(.datamods_edit_update, btn_update(ns("update")))]
        } else {
          data[, .datamods_edit_update := NA]
        }

        if (is.reactive(delete)) {
          delete <- delete()
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
          var_edit = data_rv$edit, #var_edit,
          #edit = data_rv$edit, #var_edit,
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

          new <- as.data.table(results_inputs)
          setnames(new, colnames(data))

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
          var_edit = data_rv$edit,
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


      # Download data in Excel format ---
      output$download_excel <- renderUI({
        if (is.reactive(download_excel)) {
          download_excel <- download_excel()
        }
        if (isTRUE(download_excel)) {
          tagList(
            downloadButton(
              outputId = ns("export_excel"),
              label = tagList(ph("download"), "Export data in excel format"),
              class = NULL,
              icon = NULL,
              width = "100%"
            ),
            tags$div(class = "clearfix")
          )
        }
      })

      output$export_excel <- downloadHandler(
        filename = function() {
          file_name <- file_name_export
          paste0(file_name, ".xlsx")
        },
        content = function(file) {
          data <- data_rv$data
          data <- data[, -c(".datamods_id", ".datamods_edit_update", ".datamods_edit_delete")]
          setnames(data, data_rv$colnames)
          write_xlsx(
            x = list(data = data),
            path = file
          )
        }
      )

      # Download data in csv format ---
      output$download_csv <- renderUI({
        if (is.reactive(download_csv)) {
          download_csv <- download_csv()
        }
        if (isTRUE(download_csv)) {
          tagList(
            downloadButton(
              outputId = ns("export_csv"),
              label = tagList(ph("download"), "Export data in csv format"),
              class = NULL,
              icon = NULL,
              width = "100%"
            ),
            tags$div(class = "clearfix")
          )
        }
      })
      output$export_csv <- downloadHandler(
        filename = function() {
          file_name <- file_name_export
          paste0(file_name, ".csv")
        },
        content = function(file) {
          data <- data_rv$data
          data <- data[, -c(".datamods_id", ".datamods_edit_update", ".datamods_edit_delete")]
          setnames(data, data_rv$colnames)
          write.csv(
            x = data,
            file = file
          )
        }
      )

      return(reactive(data_rv$data))

    }
  )
}

