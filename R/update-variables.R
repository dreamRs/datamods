



update_variables_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

update_variables_server <- function(input, output, session) {

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
  u <- lapply(data, uniqueN)
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
#' @param id a common id to use for text inputs, will be suffixed by row number.
#'
#' @return a \code{data.frame}
#' @noRd
#' @importFrom htmltools doRenderTags
#' @importFrom shiny textInput
set_text_input <- function(data, variable, id = "variable") {
  values <- data[[variable]]
  text_input <- mapply(
    FUN = function(inputId, value) {
      doRenderTags(textInput(
        inputId = inputId,
        label = NULL,
        value = value,
        width = "100%"
      ))
    },
    inputId = paste(id, pad(seq_along(values)), sep = "-"),
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
#' @param id a common id to use for text inputs, will be suffixed by row number.
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
    X = pad(seq_len(nrow(data)))
  )
  rownames(data) <- unlist(checkboxes, use.names = FALSE)
  data
}






#' Add select input to update class
#'
#' @param data a \code{data.frame}
#' @param variable name of the variable containing variable's class
#' @param id a common id to use for text inputs, will be suffixed by row number.
#'
#' @return a \code{data.frame}
#' @noRd
#'
#' @importFrom htmltools doRenderTags
#' @importFrom shiny selectInput
set_class_input <- function(data, variable, id = "classes") {
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
          width = "100%",
          selectize = FALSE
        ))
      } else {
        ""
      }
    },
    inputId = paste(id, pad(seq_len(nrow(data))), sep = "-"),
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
    colnames = c("Name", "Class", "Class to set", "Number of missing value", "% of complete observations", "Number of unique values"),
    selection = "none",
    escape = FALSE,
    style = "bootstrap",
    options = list(
      lengthChange = FALSE,
      paging = FALSE,
      info = FALSE,
      searching = FALSE,
      drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
      columnDefs = list(
        list(width = "100px", targets = 1),
        list(width = "100px", targets = 2),
        list(width = "100px", targets = 3)
      )
    )
  )
  formatPercentage(dt, "p_complete")
}



