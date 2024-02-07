

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
#' @param add `boolean`, if `TRUE`, allows you to add a row in the table via a button at the top right.
#' @param update `boolean`, if `TRUE`, allows you to modify a row of the table via a button located in the table on the row you want to edit.
#' @param delete `boolean`, if `TRUE`, allows a row to be deleted from the table via a button in the table.
#' @param download_csv if `TRUE`, allows to export the table in csv format via a download button.
#' @param download_excel if `TRUE`, allows to export the table in excel format via a download button.
#' @param file_name_export `character` that allows you to choose the export name of the downloaded file.
#' @param var_edit vector of `character` which allows to choose the names of the editable columns.
#' @param var_mandatory vector of `character` which allows to choose obligatory fields to fill.
#' @param var_labels named list, where names are colnames and values are labels to be used in edit modal.
#' @param add_default_values Default values to use for input control when adding new data, e.g. `list(my_var_text = "Default text to display")`.
#' @param n_column Number of column in the edit modal window, must be a number that divide 12 since it use Bootstrap grid system with [shiny::column()].
#' @param return_class Class of returned data: `data.frame`, `data.table`, `tbl_df` (tibble) or `raw`.
#' @param reactable_options Options passed to [reactable::reactable()].
#' @param modal_size `character` which allows to choose the size of the modalDialog. One of "s" for small, "m" (the default) for medium, "l" for large, or "xl" for extra large.
#' @param modal_easy_close `boolean` If TRUE, modalDialog can be dismissed by clicking outside the dialog box, or be pressing the Escape key. If FALSE (the default), modalDialog can't be dismissed in those ways; instead it must be dismissed by clicking on a modalButton(), or from a call to removeModal() on the server.
#' @param callback_add,callback_update,callback_delete Functions to be executed just before an action (add, update or delete) is performed on the data.
#'  Functions used must be like `function(data, row) {...}` where :
#'    * `data` will be the data in the table at the moment the function is called
#'    * `row` will contain either a new row of data (add), an updated row (update) or the row that will be deleted (delete).
#'
#'  If the return value of a callback function is not truthy (see [shiny::isTruthy()]) then the action is cancelled.
#' @param only_callback Only use callbacks, don't alter data within the module.
#' @param use_notify Display information or not to user through [shinybusy::notify()].
#'
#'
#'
#' @return the edited `data.frame` in reactable format with the user modifications
#'
#' @name edit-data
#'
#' @importFrom shiny moduleServer eventReactive reactiveValues is.reactive reactive renderUI actionButton observeEvent isTruthy showModal removeModal downloadButton downloadHandler
#' @importFrom data.table copy as.data.table := copy setnames as.data.table setattr
#' @importFrom reactable renderReactable reactableOutput getReactableState
#' @importFrom phosphoricons ph
#' @importFrom writexl write_xlsx
#' @importFrom utils write.csv
#' @importFrom htmltools tagList
#' @importFrom rlang is_function is_list
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
                             var_labels = NULL,
                             add_default_values = list(),
                             n_column = 1,
                             return_class = c("data.frame", "data.table", "tbl_df", "raw"),
                             reactable_options = NULL,
                             modal_size = c("m", "s", "l", "xl"),
                             modal_easy_close = TRUE,
                             callback_add = NULL,
                             callback_update = NULL,
                             callback_delete = NULL,
                             only_callback = FALSE,
                             use_notify = TRUE) {
  return_class <- match.arg(return_class)
  modal_size <- match.arg(modal_size)
  callback_default <- function(...) return(TRUE)
  if (!is_function(callback_add))
    callback_add <- callback_default
  if (!is_function(callback_update))
    callback_update <- callback_default
  if (!is_function(callback_delete))
    callback_delete <- callback_default
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
        if (is.reactive(var_labels))
          var_labels <- var_labels()
        if (is.reactive(var_edit))
          var_edit <- var_edit()
        if (is.null(var_edit))
          var_edit <- names(data)
        data <- as.data.table(data)
        data_rv$colnames <- copy(colnames(data))
        if (ncol(data) > 0) {
          setnames(data, paste0("col_", seq_along(data)))
          data_rv$internal_colnames <- copy(colnames(data))
        }
        data_rv$mandatory <- data_rv$internal_colnames[data_rv$colnames %in% var_mandatory]
        data_rv$edit <- data_rv$internal_colnames[data_rv$colnames %in% var_edit]
        data_rv$labels <- get_variables_labels(var_labels, data_rv$colnames, data_rv$internal_colnames)

        data[, .datamods_id := seq_len(.N)]

        if (is.reactive(update)) {
          update <- update()
        }

        if (isTRUE(update)) {
          data[, .datamods_edit_update := as.character(seq_len(.N))]
          data[, .datamods_edit_update := list(
            lapply(.datamods_edit_update, btn_update(ns("update")))
          )]
        } else {
          data[, .datamods_edit_update := NA]
        }

        if (is.reactive(delete)) {
          delete <- delete()
        }

        if (isTRUE(delete)) {
        data[, .datamods_edit_delete := as.character(seq_len(.N))]
        data[, .datamods_edit_delete := list(
          lapply(.datamods_edit_delete, btn_delete(ns("delete")))
        )]
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
          colnames = data_rv$colnames,
          reactable_options = reactable_options
        )
      })

      # Retrieve selected row(s)
      selected_r <- reactive({
        getReactableState("table", "selected")
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
          default = get_variables_default(
            add_default_values,
            data_rv$colnames,
            data_rv$internal_colnames
          ),
          id_validate = "add_row",
          data = data_rv$data,
          var_edit = data_rv$edit,
          var_mandatory = data_rv$mandatory,
          var_labels = data_rv$labels,
          modal_size = modal_size,
          modal_easy_close = modal_easy_close,
          n_column = n_column
        )
      })

      observeEvent(input$add_row, {
        req(data_r())
        data <- copy(data_rv$data)
        data <- as.data.table(data)

        for (var in data_rv$mandatory) {
          if (!isTruthy(input[[var]])) {
            notification_warning(
              title = i18n("Required field"),
              text = i18n("Please fill in the required fields"),
              use_notify = use_notify
            )
            return(NULL)
          }
        }

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

          res_callback <- callback_add(
            format_edit_data(data, data_rv$colnames),
            format_edit_data(new, data_rv$colnames, data_rv$internal_colnames)
          )

          if (isTruthy(res_callback) & !isTRUE(only_callback)) {
            data <- rbind(data, new[, .SD, .SDcols = !anyNA], use.names = TRUE, fill = TRUE)
            data_rv$data <- data
            update_table(data, data_rv$colnames)
            removeModal()
          } else {
            NULL
          }
        })

        if (is.null(results_add)) {
          notification_warning(
            title = i18n("Warning"),
            text = i18n("The row wasn't added to the data"),
            use_notify = use_notify
          )
        } else if (inherits(results_add, "try-error")) {
          notification_failure(
            title = i18n("Error"),
            text = i18n("Unable to add the row, contact the platform administrator"),
            use_notify = use_notify
          )
        } else {
          notification_success(
            title = i18n("Registered"),
            text = i18n("Row has been saved"),
            use_notify = use_notify
          )
        }
      })


      # Update a row ---
      observeEvent(input$update, {
        data <- copy(data_rv$data)
        data <- as.data.table(data)
        row <- data[.datamods_id == input$update]
        edit_modal(
          default = row,
          title = i18n("Update row"),
          id_validate = "update_row",
          data = data,
          var_edit = data_rv$edit,
          var_mandatory = data_rv$mandatory,
          var_labels = data_rv$labels,
          modal_size = modal_size,
          modal_easy_close = modal_easy_close,
          n_column = n_column
        )
      })

      observeEvent(input$update_row, {
        req(data_r())
        data <- copy(data_rv$data)
        data <- as.data.table(data)

        for (var in data_rv$mandatory) {
          if (!isTruthy(input[[var]])) {
            notification_failure(
              title = i18n("Required field"),
              text = i18n("Please fill in the required fields"),
              use_notify = use_notify
            )
            return(NULL)
          }
        }

        results_update <- try({
          id <- input$update

          data_updated <- copy(data)
          data_updated[.datamods_id == id, (data_rv$edit) := lapply(data_rv$edit, function(x) {
            input[[x]] %||% NA
          })]

          res_callback <- callback_update(
            format_edit_data(data, data_rv$colnames),
            format_edit_data(
              data_updated[.datamods_id == id],
              data_rv$colnames,
              data_rv$internal_colnames
            )
          )
          if (isTruthy(res_callback) & !isTRUE(only_callback)) {
            data_updated <- data_updated[order(.datamods_id)]
            data_rv$data <- copy(data_updated)
            removeModal()
            update_table(data_updated, data_rv$colnames)
          } else {
            NULL
          }
        })
        if (is.null(results_update)) {
          notification_warning(
            title = i18n("Warning"),
            text = i18n("Data wasn't updated"),
            use_notify = use_notify
          )
        } else if (inherits(results_update, "try-error")) {
          notification_failure(
            title = i18n("Error"),
            text = i18n("Unable to modify the item, contact the platform administrator"),
            use_notify = use_notify
          )
        } else {
          notification_success(
            title = i18n("Registered"),
            text = i18n("Item has been modified"),
            use_notify = use_notify
          )
        }
      })


      # Delete a row ---
      observeEvent(input$delete, {
        req(data_r())
        data <- copy(data_rv$data)
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
        data <- copy(data_rv$data)
        data <- as.data.table(data)

        results_delete <- try({

          res_callback <- callback_delete(
            format_edit_data(data, data_rv$colnames),
            format_edit_data(
              data[.datamods_id == input$delete],
              data_rv$colnames,
              data_rv$internal_colnames
            )
          )

          if (isTruthy(res_callback) & !isTRUE(only_callback)) {
            data <- data[.datamods_id != input$delete]
            data <- data[order(.datamods_id)]
            data_rv$data <- data
            removeModal()
            update_table(data, data_rv$colnames)
          } else {
            NULL
          }
        })
        if (is.null(results_delete)) {
          notification_warning(
            title = i18n("Warning"),
            text = i18n("Data wasn't deleted"),
            use_notify = use_notify
          )
        } else if (inherits(results_delete, "try-error")) {
          notification_failure(
            title = i18n("Error"),
            text = i18n("Unable to delete the row, contact platform administrator"),
            use_notify = use_notify
          )
        } else {
          notification_success(
            title = i18n("Registered"),
            text = i18n("The row has been deleted"),
            use_notify = use_notify
          )
        }
      })
      observeEvent(input$confirmation_delete_row_no, {
        notification_info(
          title = i18n("Information"),
          text = i18n("Row was not deleted"),
          use_notify = use_notify
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
          data <- format_edit_data(data_rv$data, data_rv$colnames)
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
          data <- format_edit_data(data_rv$data, data_rv$colnames)
          write.csv(
            x = data,
            file = file
          )
        }
      )


      return(
        reactive({
          req(data_rv$data)
          data <- format_edit_data(data_rv$data, data_rv$colnames)
          setattr(data, "selected", selected_r())
          as_out(data, return_class)
        })
      )

    }
  )
}

