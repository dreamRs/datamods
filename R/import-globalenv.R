
#' @title Import data from Global Environment
#'
#' @description Let the user select a dataset from its own environment.
#'
#' @param id Module's ID.
#' @param default_choices Character vector of choices to use
#'  if there's no \code{data.frame} in user's environment.
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
#' @importFrom htmltools tagList tags
#' @importFrom shiny NS actionButton icon textInput
#' @importFrom shinyWidgets pickerInput alert
#'
#' @example examples/globalenv-default.R
import_globalenv_ui <- function(id, default_choices = NULL) {

  ns <- NS(id)

  # List data.frames from environment
  choices_df <- search_obj(what = "data.frame")
  if (is.null(choices_df)) {
    choices_df <- default_choices
  }

  dataframes_dims <- get_dimensions(choices_df)

  tagList(
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

    if (!is.null(attr(choices_df, "package"))) {
      tags$div(
        style = "display: none;",
        textInput(
          inputId = ns("package"),
          value = attr(choices_df, "package"),
          label = NULL
        )
      )
    },

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


#' @param default_data Default \code{data.frame} to use.
#' @param default_name Default name to use.
#' @param update_data When to update selected data:
#'  \code{"button"} (when user click on button) or
#'  \code{"always"} (each time user select a dataset in the list).
#'
#' @export
#'
#' @importFrom shiny callModule
#'
#' @rdname import-globalenv
import_globalenv_server <- function(id,
                                    default_data = NULL,
                                    default_name = NULL,
                                    update_data = c("button", "always")) {
  callModule(
    module = import_globalenv,
    id = id,
    default_data = default_data,
    default_name = default_name,
    update_data = update_data
  )
}


#' @importFrom shiny reactiveValues observeEvent reactive removeUI
#' @importFrom htmltools tags
import_globalenv <- function(input, output, session,
                             default_data = NULL,
                             default_name = NULL,
                             update_data = c("button", "always")) {

  update_data <- match.arg(update_data)
  imported_data <- reactiveValues(data = default_data, name = default_name)
  temporary_data <- reactiveValues(data = default_data, name = default_name)

  ns <- session$ns


  if (identical(update_data, "always")) {
    removeUI(selector = paste0("#", ns("validate-button")))
  }


  observeEvent(input$data, {

    name_df <- input$data

    if (!is.null(input$package)) {
      attr(name_df, "package") <- input$package
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

      if (identical(update_data, "button")) {
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

      insert_alert(
        selector = ns("import"),
        status = "success",
        success_message
      )

      temporary_data$data <- imported
      temporary_data$name <- input$data
    }
  }, ignoreInit = TRUE)


  observeEvent(input$validate, {
    imported_data$data <- temporary_data$data
    imported_data$name <- temporary_data$name
  })


  if (identical(update_data, "button")) {
    return(list(
      data = reactive(imported_data$data),
      name = reactive(imported_data$name)
    ))
  } else {
    return(list(
      data = reactive(temporary_data$data),
      name = reactive(temporary_data$name)
    ))
  }
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
