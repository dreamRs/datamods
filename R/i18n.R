
#' @title Internationalization
#'
#' @description Simple mechanism to translate labels in a Shiny application.
#'
#' @param x Label to translate.
#' @param translations Either a `list` or a `data.frame` with translations.
#'
#' @return `i18n()` returns a `character`, `i18n_translations()` returns a `list` or a `data.frame`.
#' @export
#'
#' @name i18n
#'
#' @importFrom data.table as.data.table :=
#'
#' @example examples/i18n.R
i18n <- function(x, translations = i18n_translations()) {
  if (is.null(translations))
    return(x)
  if (is.list(translations) & !is.data.frame(translations)) {
    if (!x %in% names(translations)) {
      warning("i18n: translation for '", x, "' not found!", call. = FALSE)
      return(x)
    }
    return(translations[[x]])
  }
  if (is.data.frame(translations)) {
    translations <- as.data.table(translations)
    translations[, label := as.character(label)]
    translations <- unique(translations, by = "label")
    translations[, translation := as.character(translation)]
    if (!x %in% translations$label) {
      warning("i18n: translation for '", x, "' not found!", call. = FALSE)
      return(x)
    }
    return(translations[label == x, c(translation)])
  }
  stop("i18n option must be either: a list, a data.frame, or a path to a valid file.", call. = FALSE)
}

#' @param package Name of the package where the function is called, use `NULL` outside a package.
#'  It will retrieve option `"i18n.<PACKAGE>"` (or `"i18n"` if no package) to returns appropriate labels.
#'
#' @export
#'
#' @rdname i18n
#'
#' @importFrom utils packageName
#' @importFrom data.table fread
i18n_translations <- function(package = packageName(parent.frame(2))) {
  if (is.null(package)) {
    opts <- "i18n"
  } else {
    opts <- paste(package, "i18n", sep = ".")
  }
  language <- getOption(x = opts)
  if (is.null(language))
    return(NULL)
  if (is.character(language) && i18n_exist(language, package = package)) {
    language <- fread(file = i18n_file(language, package = package), encoding = "UTF-8", fill = TRUE)
  } else if (is.character(language) && file.exists(language)) {
    language <- fread(file = language, encoding = "UTF-8", fill = TRUE)
  } else if (is.character(language)) {
    warning(
      "i18n translations not found for : ", language,
      call. = FALSE
    )
    language <- NULL
  }
  return(language)
}

#' @param value Value to set for translation. Can be:
#'   * single `character` to use a supported language (`"fr"`, `"mk"`, `"al"`, `"pt"` for esquisse and datamods packages).
#'   * a `list` with labels as names and translations as values.
#'   * a `data.frame` with 2 column: `label` & `translation`.
#'   * path to a CSV file with same structure as for `data.frame` above.
#' @param packages Name of packages for which to set i18n, default to esquisse and datamods
#'
#' @export
#'
#' @rdname i18n
#'
#' @importFrom stats setNames
set_i18n <- function(value, packages = c("datamods", "esquisse")) {
  if (is.null(packages)) {
    options("i18n" = value)
  } else {
    i18n.opts <- setNames(
      lapply(seq_along(packages), function(...) value),
      paste(packages, "i18n", sep = ".")
    )
    options(i18n.opts)
  }
}


i18n_file <- function(x, package) {
  if (is.null(package))
    return(character(0))
  system.file("i18n", paste0(x, ".csv"), package = package)
}
i18n_exist <- function(x, package) {
  isTRUE(file.exists(i18n_file(x, package)))
}

i18n_test <- function(x) {
  i18n(x)
}
