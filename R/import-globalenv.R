
#' @title Import data from Global Environment
#'
#' @description Let the user select a dataset from its own environment.
#'
#' @param id Module's ID.
#'
#' @return
#'  * UI: HTML tags that can be included in shiny's UI
#'  * Server: a \code{list} with two slots:
#'    + **data**: a \code{reactive} function returning the selected \code{data.frame}.
#'    + **name**: a \code{reactive} function the name of the selected data as \code{character}.
#'
#'
#'
#' @export
#'
#' @name import-globalenv
#'
#' @importFrom htmltools tags
#' @importFrom shiny NS actionButton icon textInput
#' @importFrom shinyWidgets pickerInput alert
#'
#' @example examples/globalenv-default.R
import_globalenv_ui <- function(id) {

  ns <- NS(id)

  # List data.frames from environment
  choices_df <- search_obj(what = "data.frame")

  dataframes_dims <- get_dimensions(choices_df)

  tags$div(
    class = "datamods-import",
    html_dependency_datamods(),
    tags$h2("Import a dataset"),
    pickerInput(
      inputId = ns("data"),
      label = "Select a data.frame :",
      choices = choices_df,
      options = list(title = "List of data.frame..."),
      choicesOpt = list(subtext = dataframes_dims),
      width = "100%"
    ),

    tags$div(
      id = ns("import-placeholder"),
      alert(
        id = ns("import-result"),
        status = "info",
        tags$b("No data selected!"),
        "Use a data.frame from user environment",
        dismissible = TRUE
      )
    ),
    tags$div(
      id = ns("validate-button"),
      style = "margin-top: 20px;",
      actionButton(
        inputId = ns("validate"),
        label = "Import selected data",
        icon = icon("arrow-circle-right"),
        width = "100%",
        disabled = "disabled",
        class = "btn-primary"
      )
    )
  )
}



#' @param choices Character vector or \code{reactive} function
#'  returning character vector of choices to use
#'  if there's no \code{data.frame} in user's environment.
#' @param selected Default selected value, if any and if \code{choices} is provided.
#' @param trigger_return When to update selected data:
#'  \code{"button"} (when user click on button) or
#'  \code{"change"} (each time user select a dataset in the list).
#' @param return_class Class of returned data: \code{data.frame}, \code{data.table} or \code{tbl_df} (tibble).
#'
#' @export
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent reactive removeUI is.reactive icon actionLink
#' @importFrom htmltools tags tagList
#' @importFrom shinyWidgets updatePickerInput
#'
#' @rdname import-globalenv
import_globalenv_server <- function(id,
                                    choices = NULL,
                                    selected = NULL,
                                    trigger_return = c("button", "change"),
                                    return_class = c("data.frame", "data.table", "tbl_df")) {

  trigger_return <- match.arg(trigger_return)

  module <- function(input, output, session) {

    ns <- session$ns

    imported_rv <- reactiveValues(data = NULL, name = NULL)
    temporary_rv <- reactiveValues(data = NULL, name = NULL)


    if (is.reactive(choices)) {
      observeEvent(choices(), {
        updatePickerInput(
          session = session,
          inputId = "data",
          choices = choices(),
          selected = temporary_rv$name,
          choicesOpt = list(
            subtext = get_dimensions(choices())
          )
        )
        temporary_rv$package <- attr(choices(), "package")
      })
    } else {
      updatePickerInput(
        session = session,
        inputId = "data",
        choices = choices,
        selected = selected,
        choicesOpt = list(
          subtext = get_dimensions(choices)
        )
      )
      temporary_rv$package <- attr(choices, "package")
    }


    if (identical(trigger_return, "change")) {
      removeUI(selector = paste0("#", ns("validate-button")))
    }


    observeEvent(input$data, {
      req(input$data)
      name_df <- input$data

      if (!is.null(temporary_rv$package)) {
        attr(name_df, "package") <- temporary_rv$package
      }

      imported <- try(get_env_data(name_df), silent = TRUE)

      if (inherits(imported, "try-error") || NROW(imported) < 1) {

        toggle_widget(inputId = ns("validate"), enable = FALSE)

        insert_alert(
          selector = ns("import"),
          status = "danger",
          tags$b(icon("exclamation-triangle"), "Ooops"), "Something went wrong..."
        )

      } else {

        toggle_widget(inputId = ns("validate"), enable = TRUE)

        if (identical(trigger_return, "button")) {
          success_message <- tagList(
            tags$b(icon("check"), "Data ready to be imported!"),
            sprintf(
              "%s: %s obs. of %s variables imported",
              input$data, nrow(imported), ncol(imported)
            )
          )
        } else {
          success_message <- tagList(
            tags$b(icon("check"), "Data successfully imported!"),
            sprintf(
              "%s: %s obs. of %s variables imported",
              input$data, nrow(imported), ncol(imported)
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

        temporary_rv$data <- imported
        temporary_rv$name <- input$data
      }
    }, ignoreInit = TRUE)


    observeEvent(input$see_data, {
      show_data(temporary_rv$data)
    })

    observeEvent(input$validate, {
      imported_rv$data <- temporary_rv$data
      imported_rv$name <- temporary_rv$name
    })


    if (identical(trigger_return, "button")) {
      return(list(
        data = reactive(as_out(imported_rv$data, return_class)),
        name = reactive(imported_rv$name)
      ))
    } else {
      return(list(
        data = reactive(as_out(temporary_rv$data, return_class)),
        name = reactive(temporary_rv$name)
      ))
    }
  }

  moduleServer(
    id = id,
    module = module
  )
}







# utils -------------------------------------------------------------------

#' List dateset contained in a package
#'
#' @param pkg Name of the package, must be installed.
#'
#' @return a \code{character} vector or \code{NULL}.
#' @export
#'
#' @importFrom utils data
#'
#' @examples
#'
#' list_pkg_data("ggplot2")
list_pkg_data <- function(pkg) {
  if (isTRUE(requireNamespace(pkg, quietly = TRUE))) {
    list_data <- data(package = pkg, envir = environment())$results[, "Item"]
    attr(list_data, "package") <- pkg
    list_data
  } else {
    NULL
  }
}

#' @importFrom utils data
get_env_data <- function(obj, env = globalenv()) {
  if (obj %in% ls(name = env)) {
    get(x = obj, envir = env)
  } else if (!is.null(attr(obj, "package")) && !identical(attr(obj, "package"), "")) {
    get(utils::data(list = obj, package = attr(obj, "package"), envir = environment()))
  } else {
    NULL
  }
}


get_dimensions <- function(objs) {
  if (is.null(objs))
    return(NULL)
  dataframes_dims <- Map(
    f = function(name, pkg) {
      attr(name, "package") <- pkg
      tmp <- get_env_data(name)
      sprintf("%d obs. of  %d variables", nrow(tmp), ncol(tmp))
    },
    name = objs,
    pkg = if (!is.null(attr(objs, "package"))) {
      attr(objs, "package")
    } else {
      character(1)
    }
  )
  unlist(dataframes_dims)
}
