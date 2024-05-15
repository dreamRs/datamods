#' Function to extract labels
#'
#' @param folder file directory
#'
#' @return an extraction of the labels contained in the directory files
#' @importFrom stringr str_subset str_extract_all str_remove_all
#' @export
#'
#' @examples extract_labels(folder = "R")
extract_labels <- function(folder = "R") {
  files <- list.files(folder)
  list_extractions <- sapply(
    X = files,
    FUN = function(file) {
      read_file <- readLines(file.path(folder, file))
      extraction <- str_extract_all(
        string = str_subset(read_file, "i18n"),
        pattern = 'i18n\\("[[:graph:][:space:]-["\\)]]*"\\)'
      ) |>
        unlist()
      extraction
    }
  )
  extract_labels <- character(0)
  for (i in seq_along(list_extractions)) {
    extraction <- list_extractions[[i]]
    extract_labels <- c(extract_labels, extraction)
  }
  str_remove_all(unique(extract_labels), paste(c("i18n", "\"", "\\)", "\\("), collapse = "|"))
}



#' Update all csvs that are in inst/i18n
#'
#' @param labels results of label extractions
#'
#' @return all csvs updated
#' @importFrom data.table merge fwrite data.table fread unique
#' @export
#'
#' @examples update_csv(labels = c(extract_labels(folder = "R"), extract_labels(folder = "examples")))
#' new_csv_fr <- fread("inst/i18n/fr.csv")
update_csv <- function(labels,
                       lang,
                       ...) {
  old <- fread(file = sprintf("inst/i18n/%s.csv", lang), encoding = "UTF-8", fill = TRUE)
  new <- merge(
    x = data.table(label = unique(new_labels)),
    y = old,
    by = "label",
    all.x = TRUE
  )

  final <- rbind(
    new[!is.na(translation)],
    translate_labels(new[is.na(translation)]$label, target_language = lang, ...)
  )

  final[] # ecrire csv
}



#' Translate labels
#'
#' @param labels labels to translate
#' @param source_language the language that you want to translate the text into
#' @param target_language the language of the text that you want to translate
#' @param encoding Name of encoding. See stringi::stri_enc_list() for a complete list
#'
#' @importFrom polyglotr google_translate
#' @importFrom stringr str_conv
#' @importFrom data.table data.table
#'
#' @return a data frame with translated labels
#' @export
#'
#' @examples translate_labels(labels = extract_labels(folder = "R"))
translate_labels <- function(labels,
                             source_language = "en",
                             target_language = "fr",
                             encoding = "UTF-8") {

  translation <- polyglotr::google_translate(
    text = labels,
    target_language = target_language,
    source_language = source_language
  )
  data.table(
    label = labels,
    translation = translation |>
      unlist() |>
      str_conv(encoding),
    comment = "Automacally translated"
  )
}
# Informations sur le package {polyglotr}
# https://github.com/Tomeriko96/polyglotr/
# install.packages("polyglotr")
# Table avec les codes des langages disponibles
# google_supported_languages
# Liste des encodages
# encodage <- data.frame(encoding = stringi::stri_enc_list())



#' Select translation
#'
#' @param file csv file
#' @param labels labels to translate
#'
#' @importFrom stringr str_remove
#'
#' @return a data frame with translated labels according to the language of the csv file
#' @export
#'
#' @examples select_translation(file = "fr.csv")
select_translation <- function(file,
                               labels = extract_labels(folder = "R")) {

  file <- str_remove(file, ".csv")

  if (identical(file, "fr")) {
    translate_labels(labels = labels, target_language = "fr")
  } else if (identical(file, "es")) {
    translate_labels(labels = labels, target_language = "es")
  } else if (identical(file, "de")) {
    translate_labels(labels = labels, target_language = "de")
  } else if (identical(file, "al")) {
    translate_labels(labels = labels, target_language = "sq")
  } else if (identical(file, "pl")) {
    translate_labels(labels = labels, target_language = "pl")
  } else if (identical(file, "pt")) {
    translate_labels(labels = labels, target_language = "pt")
  } else if (identical(file, "tr")) {
    translate_labels(labels = labels, target_language = "tr")
  } else if (identical(file, "mk")) {
    translate_labels(labels = labels, target_language = "mk")  # revoir encoding
  } else if (identical(file, "ja")) {
    translate_labels(labels = labels, target_language = "ja", encoding = "ISO_2022,locale=ja,version=4")
  } else if (identical(file, "cn")) {
    translate_labels(labels = labels, target_language = "zh-CN", encoding = "chinese") # "GBK", "UTF16_BigEndian", "GB18030"
  } else if (identical(file, "ko")) {
    translate_labels(labels = labels, target_language = "ko", encoding = "csKOI8R") # "GBK", "korean"
  } else {
    NULL
  }
}
