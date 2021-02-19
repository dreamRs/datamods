

#' @importFrom data.table as.data.table fread
i18n <- function(x) {
  lang <- getOption("datamods.i18n")
  if (is.null(lang))
    return(x)
  if (is.list(lang) & !is.data.frame(lang)) {
    if (!x %in% names(lang)) {
      warning("datamods i18n: translation for '", x, "' not found!", call. = FALSE)
      return(x)
    }
    return(lang[[x]])
  }
  if (is.character(lang) && i18n_exist(lang)) {
    lang <- fread(file = i18n_file(lang), encoding = "UTF-8")
    # options("datamods.i18n" = lang)
  }
  if (is.character(lang) && file.exists(lang)) {
    lang <- fread(file = lang, encoding = "UTF-8")
    # options("datamods.i18n" = lang)
  }
  if (is.data.frame(lang)) {
    lang <- as.data.table(lang)
    lang <- unique(lang, by = "label")
    if (!x %in% lang$label) {
      warning("datamods i18n: translation for '", x, "' not found!", call. = FALSE)
      return(x)
    }
    return(lang[label == x, c(translation)])
  }
  stop("datamods.i18n must be either: a list, a data.frame, or a path to a valid file.", call. = FALSE)
}

i18n_file <- function(x) {
  system.file("i18n", paste0(x, ".csv"), package = "datamods")
}
i18n_exist <- function(x) {
  file.exists(i18n_file(x))
}
