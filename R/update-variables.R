
#' Select, rename and convert variables
#'
#' @param id Module's ID.
#'
#' @return
#' @export
#'
#' @name update-variables
#'
#' @importFrom shiny uiOutput actionButton icon
#' @importFrom htmltools tagList tags
#' @importFrom DT DTOutput
#' @importFrom shinyWidgets html_dependency_pretty
#'
#' @examples
update_variables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    html_dependency_pretty(),
    tags$h3("Update variables"),
    uiOutput(outputId = ns("data_info")),
    DTOutput(outputId = ns("table")),
    tags$br(),
    tags$div(
      id = ns("update-placeholder"),
      alert(
        id = ns("update-result"),
        status = "info",
        icon("info"), "Select, rename and convert variables in table above, then apply changes by clicking button below."
      )
    ),
    actionButton(
      inputId = ns("validate"),
      label = "Apply changes",
      icon = icon("arrow-circle-right"),
      width = "100%",
      class = "btn-primary"
    )
  )
}

#' @export
#'
#' @param id Module's ID
#' @param data a \code{data.frame}
#'
#' @rdname update-variables
#'
#' @importFrom shiny callModule
update_variables_server <- function(id, data) {
  callModule(
    module = update_variables,
    id = id,
    data = data
  )
}

#' @importFrom shiny reactiveValues reactive renderUI reactiveValuesToList
#' @importFrom DT renderDT
update_variables <- function(input, output, session,
                             data) {
  ns <- session$ns
  updated_data <- reactiveValues(x = NULL)

  data_r <- reactive({
    if (is.reactive(data)) {
      data()
    } else {
      data
    }
  })

  output$data_info <- renderUI({
    data <- data_r()
    sprintf("Data has %s observations and %s variables", nrow(data), ncol(data))
  })

  variables_r <- reactive({
    data <- data_r()
    updated_data$x <- NULL
    summary_vars(data)
  })

  output$table <- renderDT({
    req(variables_r())
    variables <- variables_r()
    variables <- set_checkbox(variables, ns("selection"))
    variables <- set_text_input(variables, "name", ns("name"))
    variables <- set_class_input(variables, "class", ns("class_to_set"))
    update_variables_datatable(variables)
  })

  observeEvent(input$validate, {
    data <- data_r()

    # getting the input values
    new_names <- get_inputs("name")
    new_classes <- get_inputs("class_to_set")
    new_selections <- get_inputs("selection")

    data_sv <- variables_r()
    vars_to_change <- get_vars_to_convert(data_sv, new_classes)

    # rename
    names(data) <- unlist(new_names, use.names = FALSE)

    # convert
    if (nrow(vars_to_change) > 0) {
      data <- convert_to(
        data = data,
        variable = vars_to_change$name,
        new_class = vars_to_change$class_to_set
      )
    }

    # select
    data <- data[, unlist(new_selections, use.names = FALSE)]

    updated_data$x <- data
  })

  return(reactive(updated_data$x))
}




# utils -------------------------------------------------------------------


#' Get variables classes from a \code{data.frame}
#'
#' @param data a \code{data.frame}
#'
#' @return a \code{character} vector as same length as number of variables
#' @noRd
#'
#' @examples
#'
#' get_classes(mtcars)
get_classes <- function(data) {
  classes <- lapply(
    X = data,
    FUN = function(x) {
      paste(class(x), collapse = ", ")
    }
  )
  unlist(classes, use.names = FALSE)
}


#' Get count of unique values in variables of \code{data.frame}
#'
#' @param data a \code{data.frame}
#'
#' @return a \code{numeric} vector as same length as number of variables
#' @noRd
#'
#' @importFrom data.table uniqueN
#'
#' @examples
get_n_unique <- function(data) {
  u <- lapply(data, FUN = function(x) {
    if (is.atomic(x)) {
      uniqueN(x)
    } else {
      NA_integer_
    }
  })
  unlist(u, use.names = FALSE)
}



#' Add padding 0 to a vector
#'
#' @param x a \code{vector}
#'
#' @return a \code{character} vector
#' @noRd
#'
#' @examples
#'
#' pad0(1:10)
#' pad0(c(1, 15, 150, NA))
pad0 <- function(x) {
  NAs <- which(is.na(x))
  x <- formatC(x, width = max(nchar(as.character(x)), na.rm = TRUE), flag = "0")
  x[NAs] <- NA
  x
}



#' Variables summary
#'
#' @param data a \code{data.frame}
#'
#' @return a \code{data.frame}
#' @noRd
#'
#' @examples
#'
#' summary_vars(iris)
#' summary_vars(mtcars)
summary_vars <- function(data) {
  data <- as.data.frame(data)
  datsum <- data.frame(
    name = names(data),
    class = get_classes(data),
    n_missing = unname(colSums(is.na(data))),
    stringsAsFactors = FALSE
  )
  datsum$p_complete <- 1 - datsum$n_missing / nrow(data)
  datsum$n_unique <- get_n_unique(data)
  datsum
}




#' @title Convert to textInput
#'
#' @description Convert a variable to several text inputs to be displayed in a \code{DT::datatable}.
#'
#' @param data a \code{data.frame}
#' @param variable name of the variable to replace by text inputs
#' @param id a common id to use for text inputs, will be suffixed by row number
#' @param width width of input
#'
#' @return a \code{data.frame}
#' @noRd
#' @importFrom htmltools doRenderTags
#' @importFrom shiny textInput
set_text_input <- function(data, variable, id = "variable", width = "100%") {
  values <- data[[variable]]
  text_input <- mapply(
    FUN = function(inputId, value) {
      doRenderTags(textInput(
        inputId = inputId,
        label = NULL,
        value = value,
        width = width
      ))
    },
    inputId = paste(id, pad0(seq_along(values)), sep = "-"),
    value = values,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  data[[variable]] <- text_input
  data
}




#' Convert rownames to checkboxes
#'
#' @param data a \code{data.frame}
#' @param id a common id to use for text inputs, will be suffixed by row number
#'
#' @return a \code{data.frame}
#' @noRd
#'
#' @importFrom htmltools tagAppendAttributes doRenderTags
#' @importFrom shinyWidgets prettyCheckbox
#'
#' @note
#' \code{shinyWidgets::prettyCheckbox} HTML dependency need to be included manually.
set_checkbox <- function(data, id = "selection") {
  checkboxes <- lapply(
    FUN = function(i) {
      tag <- prettyCheckbox(
        inputId = paste(id, i, sep = "-"),
        label = NULL,
        value = TRUE,
        status = "primary",
        plain = TRUE,
        icon = shiny::icon("check"),
        width = "100%"
      )
      tag <- tagAppendAttributes(tag, style = "margin-top: 5px;")
      doRenderTags(tag)
    },
    X = pad0(seq_len(nrow(data)))
  )
  rownames(data) <- unlist(checkboxes, use.names = FALSE)
  data
}






#' Add select input to update class
#'
#' @param data a \code{data.frame}
#' @param variable name of the variable containing variable's class
#' @param id a common id to use for text inputs, will be suffixed by row number
#' @param width width of input
#'
#' @return a \code{data.frame}
#' @noRd
#'
#' @importFrom htmltools doRenderTags
#' @importFrom shiny selectInput
set_class_input <- function(data, variable, id = "classes", width = "120px") {
  classes <- data[[variable]]
  classes_up <- c("character", "factor", "numeric", "integer", "date", "datetime")
  class_input <- mapply(
    FUN = function(inputId, class) {
      if (class %in% classes_up) {
        doRenderTags(selectInput(
          inputId = inputId,
          label = NULL,
          choices = classes_up,
          selected = class,
          width = width,
          selectize = FALSE
        ))
      } else {
        ""
      }
    },
    inputId = paste(id, pad0(seq_len(nrow(data))), sep = "-"),
    class = classes,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  datanames <- names(data)
  datanames <- append(
    x = datanames,
    values = paste0(variable, "_toset"),
    after = which(datanames == variable)
  )
  data[[paste0(variable, "_toset")]] <- class_input
  data[, datanames]
}



#' DT table to display variables info & update
#'
#' @param data a \code{data.frame}
#'
#' @return a \code{DT} htmlwidget
#' @noRd
#'
#' @importFrom DT datatable formatPercentage JS
update_variables_datatable <- function(data) {

  dt <- datatable(
    data = data,
    rownames = TRUE,
    colnames = c("Name", "Class", "Class to set",
                 "Missing value",
                 "Complete observations",
                 "Unique values"),
    selection = "none",
    escape = FALSE,
    style = "bootstrap",
    class = "display dt-responsive",
    options = list(
      scrollY = "400px",
      scrollX = TRUE,
      lengthChange = FALSE,
      paging = FALSE,
      info = FALSE,
      searching = FALSE,
      drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
      columnDefs = list(
        list(width = "100px", targets = list(1))
      )
      # columns = list(
      #   list(width = "100px"),
      #   list(width = "100px"),
      #   list(width = "100px"),
      #   NULL, NULL, NULL
      # )
    )
  )
  formatPercentage(dt, "p_complete")
}





#' Retrieve all inputs according to pattern
#'
#' @param pattern Pattern to search for
#' @param session Shiny session
#'
#' @return a list
#' @noRd
#'
#' @importFrom shiny getDefaultReactiveDomain isolate
#'
get_inputs <- function(pattern, session = shiny::getDefaultReactiveDomain()) {
  all_inputs <- isolate(reactiveValuesToList(session$input))
  filtered <- sort(names(all_inputs))
  filtered <- grep(pattern = pattern, x = filtered, value = TRUE)
  all_inputs[filtered]
}



#' Convert a variable to specific new class
#'
#' @param data A \code{data.frame}
#' @param variable Name of the variable to convert
#' @param new_class Class to set
#' @param ... Other arguments passed on to methods.
#'
#' @return A \code{data.frame}
#' @noRd
#'
#' @examples
#' dat <- data.frame(
#'   v1 = month.name,
#'   v2 = month.abb,
#'   v3 = 1:12,
#'   v4 = as.numeric(Sys.Date() + 0:11),
#'   v5 = as.character(Sys.Date() + 0:11),
#'   v6 = as.factor(c("a", "a", "b", "a", "b", "a", "a", "b", "a", "b", "b", "a")),
#'   v7 = as.character(11:22),
#'   stringsAsFactors = FALSE
#' )
#'
#' str(dat)
#'
#' str(convert_to(dat, "v3", "character"))
#' str(convert_to(dat, "v6", "character"))
#' str(convert_to(dat, "v7", "numeric"))
#' str(convert_to(dat, "v4", "date", origin = "1970-01-01"))
#' str(convert_to(dat, "v5", "date"))
#'
#' str(convert_to(dat, c("v1", "v3"), c("factor", "character")))
#'
#' str(convert_to(dat, c("v1", "v3", "v4"), c("factor", "character", "date"), origin = "1970-01-01"))
#'
convert_to <- function(data,
                       variable,
                       new_class = c("character", "factor", "numeric", "integer", "date", "datetime"),
                       ...) {
  new_class <- match.arg(new_class, several.ok = TRUE)
  stopifnot(length(new_class) == length(variable))
  if (length(variable) > 1) {
    for (i in seq_along(variable)) {
      data <- convert_to(data, variable[i], new_class[i], ...)
    }
    return(data)
  }
  if (identical(new_class, "character")) {
    data[[variable]] <- as.character(x = data[[variable]], ...)
  } else if (identical(new_class, "factor")) {
    data[[variable]] <- as.factor(x = data[[variable]])
  } else if (identical(new_class, "numeric")) {
    data[[variable]] <- as.numeric(x = data[[variable]], ...)
  } else if (identical(new_class, "integer")) {
    data[[variable]] <- as.integer(x = data[[variable]], ...)
  } else if (identical(new_class, "date")) {
    data[[variable]] <- as.Date(x = data[[variable]], ...)
  } else if (identical(new_class, "datetime")) {
    data[[variable]] <- as.POSIXct(x = data[[variable]], ...)
  }
  return(data)
}




#' Extract variable ID from input names
#'
#' @param x Character vector
#'
#' @return a character vector
#' @noRd
#'
#' @examples
#' extract_id(c("class_to_set-01", "class_to_set-02",
#'              "class_to_set-03", "class_to_set-04",
#'              "class_to_set-05", "no-id-there"))
extract_id <- function(x) {
  match_reg <- regexec(pattern = "(?<=-)\\d+$", text = x, perl = TRUE)
  result <- regmatches(x = x, m = match_reg)
  result <- lapply(
    X = result,
    FUN = function(x) {
      if (length(x) < 1) {
        NA_character_
      } else {
        x
      }
    }
  )
  unlist(result)
}







#' Get variable(s) to convert
#'
#' @param vars Output of \code{summary_vars}
#' @param classes_input List of inputs containing new classes
#'
#' @return a \code{data.frame}.
#' @noRd
#'
#' @examples
#' # 2 variables to convert
#' new_classes <- list(
#'   "class_to_set-1" = "numeric",
#'   "class_to_set-2" = "numeric",
#'   "class_to_set-3" = "character",
#'   "class_to_set-4" = "numeric",
#'   "class_to_set-5" = "character"
#' )
#' get_vars_to_convert(summary_vars(iris), new_classes)
#'
#'
#' # No changes
#' new_classes <- list(
#'   "class_to_set-1" = "numeric",
#'   "class_to_set-2" = "numeric",
#'   "class_to_set-3" = "numeric",
#'   "class_to_set-4" = "numeric",
#'   "class_to_set-5" = "factor"
#' )
#' get_vars_to_convert(summary_vars(iris), new_classes)
#'
#'
#' new_classes <- list(
#'   "class_to_set-01" = "character",
#'   "class_to_set-02" = "numeric",
#'   "class_to_set-03" = "character",
#'   "class_to_set-04" = "numeric",
#'   "class_to_set-05" = "character",
#'   "class_to_set-06" = "character",
#'   "class_to_set-07" = "numeric",
#'   "class_to_set-08" = "character",
#'   "class_to_set-09" = "numeric",
#'   "class_to_set-10" = "character",
#'   "class_to_set-11" = "integer"
#' )
#' get_vars_to_convert(summary_vars(mtcars), new_classes)
get_vars_to_convert <- function(vars, classes_input) {
  classes_input <- data.frame(
    id = names(classes_input),
    class_to_set = unlist(classes_input, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  classes_input$id <- extract_id(classes_input$id)
  vars$id <- pad0(seq_len(nrow(vars)))
  classes_df <- merge(x = vars, y = classes_input, by = "id")
  classes_df <- classes_df[!is.na(classes_df$class_to_set), ]
  with(classes_df, classes_df[class != class_to_set, ])
}








