
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}

nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0 || x == ""
}

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

dropListColumns <- function(x) {
  type_col <- vapply(X = x, FUN = typeof, FUN.VALUE = character(1),
                     USE.NAMES = FALSE)
  x[, type_col != "list", drop = FALSE]
}

#' Search for object with specific class in an environment
#'
#' @param what a class to look for
#' @param env An environment
#'
#' @return Character vector of the names of objects, NULL if none
#' @noRd
#'
#' @examples
#'
#' # NULL if no data.frame
#' search_obj("data.frame")
#'
#' library(ggplot2)
#' data("mpg")
#' search_obj("data.frame")
#'
#'
#' gg <- ggplot()
#' search_obj("ggplot")
#'
search_obj <- function(what = "data.frame", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if (inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    }
  )
  objs <- unlist(objs)
  if (length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}




#' @importFrom data.table as.data.table
#' @importFrom tibble as_tibble
as_out <- function(x, return_class = c("data.frame", "data.table", "tbl_df")) {
  if (is.null(x))
    return(NULL)
  return_class <- match.arg(return_class)
  is_sf <- inherits(x, "sf")
  x <- if (identical(return_class, "data.frame")) {
    as.data.frame(x)
  } else if (identical(return_class, "data.table")) {
    as.data.table(x)
  } else {
    as_tibble(x)
  }
  if (is_sf)
    class(x) <- c("sf", class(x))
  return(x)
}


genId <- function(bytes = 12) {
  paste(format(as.hexmode(sample(256, bytes, replace = TRUE) - 1), width = 2), collapse = "")
}

makeId <- function(x) {
  x <- as.character(x)
  x <- lapply(X = x, FUN = function(y) {
    paste(as.character(charToRaw(y)), collapse = "")
  })
  x <- unlist(x, use.names = FALSE)
  make.unique(x, sep = "_")
}


