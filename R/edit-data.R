

#' @title Shiny module to interactively edit a `data.frame`
#'
#' @description The module generates different options to edit a `data.frame`: adding, deleting and modifying rows, exporting data (csv and excel), choosing editable columns, choosing mandatory columns.
#' This module returns the edited table with the user modifications.
#'
#' @param id Module ID
#'
#' @importFrom shiny uiOutput
#' @importFrom htmltools tagList tags
#' @importFrom reactable reactableOutput
#' @importFrom utils getFromNamespace
#'
#' @export
#'
#' @name edit-data
#'
#' @example examples/edit_data.R
edit_data_ui <- function(id) {
  ns <- NS(id)

  notify_dep <- getFromNamespace("html_dependency_notify", "shinybusy")

  tagList(

    notify_dep(),

    # Download data in Excel format --
    uiOutput(outputId = ns("download_excel"), style = "display: inline;"),

    # Download data in csv format --
    uiOutput(outputId = ns("download_csv"), style = "display: inline;"),

    # Add a row --
    uiOutput(outputId = ns("add_button"), style = "display: inline;"),

    tags$div(class = "clearfix mb-2"),

    # Table --
    reactableOutput(outputId = ns("table"))
  )
}

#' @title Shiny module to interactively edit a `data.frame`
#'
#' @param id Module ID
#' @param data_r data_r `reactive` function containing a `data.frame` to use in the module.
#' @param add `boolean`, if `TRUE`, allows you to add a row in the table via a button at the top right
#' @param update `boolean`, if `TRUE`, allows you to modify a row of the table via a button located in the table on the row you want to edit
#' @param delete `boolean`, if `TRUE`, allows a row to be deleted from the table via a button in the table
#' @param download_csv if `TRUE`, allows to export the table in csv format via a download button
#' @param download_excel if `TRUE`, allows to export the table in excel format via a download button
#' @param file_name_export `character` that allows you to choose the export name of the downloaded file
#' @param var_edit vector of `character` which allows to choose the names of the editable columns
#' @param var_mandatory vector of `character` which allows to choose obligatory fields to fill
#' @param return_class Class of returned data: `data.frame`, `data.table`, `tbl_df` (tibble) or `raw`.
#'
#' @return the edited `data.frame` in reactable format with the user modifications
#'
#' @name edit-data
#'
#' @importFrom shiny moduleServer eventReactive reactiveValues is.reactive reactive renderUI actionButton observeEvent isTruthy showModal removeModal downloadButton downloadHandler
#' @importFrom data.table copy as.data.table := copy setnames as.data.table
#' @importFrom reactable renderReactable reactableOutput getReactableState updateReactable
#' @importFrom phosphoricons ph
#' @importFrom writexl write_xlsx
#' @importFrom utils write.csv
#' @importFrom htmltools tagList
#'
#' @export
#'
edit_data_server <- function(id,
                             data_r = reactive(NULL),
                             add = TRUE,
                             update = TRUE,
                             delete = TRUE,
                             download_csv = TRUE,
                             download_excel = TRUE,
                             file_name_export = "data",
                             var_edit = NULL,
                             var_mandatory = NULL,
                             return_class = c("data.frame", "data.table", "tbl_df", "raw")) {
  return_class <- match.arg(return_class)
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      data_rv <- reactiveValues(data = NULL, colnames = NULL, mandatory = NULL, edit = NULL)

      # Data data_r() with added columns ".datamods_edit_update" et ".datamods_edit_delete" ---
      data_init_r <- eventReactive(data_r(), {
        req(data_r())
        data <- data_r()
        if (is.reactive(var_mandatory))
          var_mandatory <- var_mandatory()
        if (is.reactive(var_edit))
          var_edit <- var_edit()
        if (is.null(var_edit))
          var_edit <- names(data)
        data <- as.data.table(data)
        data_rv$colnames <- copy(colnames(data))
        if (!isTRUE(identical(x = seq_along(data), y = integer(0)))) {
          setnames(data, paste0("col_", seq_along(data)))
        }
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
          actionButton(
            inputId = ns("add"),
            label = tagList(ph("plus"), i18n("Add a row")),
            class = "btn-outline-primary float-end"
          )
        }
      })

      observeEvent(input$add, {
        req(data_r())
        edit_modal(
          id_validate = "add_row",
          data = data_rv$data,
          colnames = data_rv$colnames,
          var_edit = data_rv$edit,
          var_mandatory = var_mandatory
        )
      })

      observeEvent(input$add_row, {
        req(data_r())
        data <- data_rv$data
        data <- as.data.table(data)

        for (var in data_rv$mandatory) {
          if (!isTruthy(input[[var]])) {
            notification_warning(
              title = i18n("Required field"),
              text = i18n("Please fill in the required fields")
            )
            return(NULL)
          }
        }

        removeModal()

        results_add <- try({
          results_inputs <- lapply(
            X = setNames(data_rv$edit, data_rv$edit),
            FUN = function(x) {
              input[[x]] %||% NA
            }
          )
          id <- max(data$.datamods_id) + 1
          results_inputs[[".datamods_id"]] <- id
          results_inputs[[".datamods_edit_update"]] <- if (update) list(btn_update(ns("update"))(id)) else NA
          results_inputs[[".datamods_edit_delete"]] <- if (delete) list(btn_delete(ns("delete"))(id)) else NA

          new <- as.data.table(results_inputs)

          data <- rbind(data, new, fill = TRUE)
          data_rv$data <- data
          page <- getReactableState(outputId = "table", name = "page")
          updateReactable("table", data = data, page = page)
        })
        if (inherits(results_add, "try-error")) {
          notification_failure(
            title = i18n("Error"),
            text = i18n("Unable to add the row, contact the platform administrator")
          )
        } else {
          notification_success(
            title = i18n("Registered"),
            text = i18n("Row has been saved")
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
          title = i18n("Update row"),
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

        for (var in data_rv$mandatory) {
          if (!isTruthy(input[[var]])) {
            notification_failure(
              title = i18n("Required field"),
              text = i18n("Please fill in the required fields")
            )
            return(NULL)
          }
        }

        removeModal()

        results_update <- try({
          id <- input$update
          data[.datamods_id == id, (data_rv$edit) := lapply(data_rv$edit, function(x) {
            input[[x]] %||% NA
          })]
          data <- data[order(.datamods_id)]
          data_rv$data <- copy(data)
          page <- getReactableState(outputId = "table", name = "page")
          updateReactable("table", data = data, page = page)
        })
        if (inherits(results_update, "try-error")) {
          notification_failure(
            title = i18n("Error"),
            text = i18n("Unable to modify the item, contact the platform administrator")
          )
        } else {
          notification_success(
            title = i18n("Registered"),
            text = i18n("Item has been modified")
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
          title = i18n("Delete"),
          i18n("Do you want to delete the selected row ?")
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
          notification_failure(
            title = i18n("Error"),
            text = i18n("Unable to delete the row, contact platform administrator")
          )
        } else {
          notification_success(
            title = i18n("Registered"),
            text = i18n("The row has been deleted")
          )
        }
        removeModal()
      })
      observeEvent(input$confirmation_delete_row_no, {
        notification_info(
          title = i18n("Information"),
          text = i18n("Row was not deleted")
        )
        removeModal()
      })


      # Download data in Excel format ---
      output$download_excel <- renderUI({
        if (is.reactive(download_excel)) {
          download_excel <- download_excel()
        }
        if (isTRUE(download_excel)) {
          downloadButton(
            outputId = ns("export_excel"),
            label = tagList(ph("download"), "Excel"),
            icon = NULL,
            class = "btn-datamods-export"
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
          data <- as.data.table(data)
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
          downloadButton(
            outputId = ns("export_csv"),
            label = tagList(ph("download"), "CSV"),
            icon = NULL,
            class = "btn-datamods-export"
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
          data <- as.data.table(data)
          data <- data[, -c(".datamods_id", ".datamods_edit_update", ".datamods_edit_delete")]
          setnames(data, data_rv$colnames)
          write.csv(
            x = data,
            file = file
          )
        }
      )

      return(
        reactive({
          req(data_rv$data)
          data <- data_rv$data
          data <- as.data.table(data)
          data <- data[,-c(".datamods_id", ".datamods_edit_update", ".datamods_edit_delete")]
          setnames(data, data_rv$colnames)
          as_out(data, return_class)
        })
      )

    }
  )
}

