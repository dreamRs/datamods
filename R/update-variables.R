
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
        icon("info"), "Select, rename and convert variables"
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

  output$data_info <- renderUI({
    if (is.reactive(data)) {
      data <- data()
    }
    sprintf("Data has %s observations and %s variables", nrow(data), ncol(data))
  })

  variables <- reactive({
    if (is.reactive(data)) {
      updated_data$x <- NULL
      summary_vars(data())
    } else {
      summary_vars(data)
    }
  })

  output$table <- renderDT({
    req(variables())
    variables <- variables()
    variables <- set_checkbox(variables, ns("selection"))
    variables <- set_text_input(variables, "name", ns("name"))
    variables <- set_class_input(variables, "class", ns("class_to_set"))
    update_variables_datatable(variables)
  })
  
  observeEvent(input$validate, {
    if (is.reactive(data)) {
      data <- data()
    }
    inputs <- sort(names(reactiveValuesToList(input)))
    print(inputs)
    ## getting the inputs
    input_names <- grep("name", inputs, value = TRUE)
    input_classes <- grep("class", inputs, value = TRUE)
    input_selections <- grep("selection", inputs, value = TRUE)
    
    ## getting the input values
    changed_names <- sapply(input_names, function(x) input[[x]])
    changed_classes <- sapply(input_classes, function(x) input[[x]])
    changed_selections <- sapply(input_selections, function(x) input[[x]])
    
    set_class <- function(col, fun) {
      cat(names(col), sep = "-")
      cat(fun, sep = "\n")
      if (fun %in% c("character", "factor", "numeric", "date", "datetime"))
        sapply(col, paste0("as.", fun))
      else col
    }
    
    ## apply changes
    n <- length(input_classes)
    print(input_classes)
    
    names(data) <- changed_names
    data <- data[, changed_selections]
    
    data <- data.frame(lapply(1:n, function(i) set_class(data[, i], changed_classes[i])))

    tibble::glimpse(data)
  })

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
#' get_class(mtcars)
get_class <- function(data) {
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
    class = get_class(data),
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
  classes_up <- c("character", "factor", "numeric", "date", "datetime")
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
        doRenderTags(selectInput(
          inputId = inputId,
          label = NULL,
          choices = "Not applicable",
          selected = class,
          width = width,
          selectize = FALSE
        ))
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



