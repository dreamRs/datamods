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
  files <- list.files( file.path("../datamods", folder))
  list_extractions <- sapply(
    X = files,
    FUN = function(file) {
      read_file <- readLines(file.path(folder, file))
      extraction <- str_extract_all(
        string = str_subset(read_file, "i18n"),
        pattern = 'i18n\\("[[:graph:][:space:]-["\\)]]*"\\)'
      ) %>% 
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
#' @return all csvs updated
#' @importFrom readr read_csv
#' @importFrom utils write.csv
#' @importFrom dplyr full_join
#' @export
#'
#' @examples update_csv()
#' new_fr_csv <- fread("inst/i18n/fr.csv")
update_csv <- function() {
  # results of label extractions
  labels <- c(extract_labels(folder = "R"), extract_labels(folder = "examples"))
  
  # rewriting csv files with the extracted labels and the old ones
  files <- setdiff(list.files(file.path("../datamods", "inst", "i18n")), "extract_labels.R") 
  for (i in seq_along(files)) {
    path <- file.path("inst", "i18n", files[i])
    old_csv <- read_csv(file = path)
    join <- full_join(
      x = old_csv,
      y = data.frame(label = labels),
      by = c("label")
    )
    write.csv(join, file = path, row.names = FALSE)
  }
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
  data.frame(
    label = labels,
    translation = translation %>% 
      unlist() %>% 
      str_conv(encoding) 
  ) 
}
# Informations sur le package {polyglotr}
# https://github.com/Tomeriko96/polyglotr/
# install.packages("polyglotr")
# Table avec les codes des langages disponibles
# google_supported_languages
# Liste des encodages
# encodage <- data.frame(encoding = stringi::stri_enc_list())

# Exemples 

# # francais
# translate_labels(labels = extract_labels(folder = "R"))
# # espagnol
# translate_labels(labels = extract_labels(folder = "R"), target_language = "es")
# # allemand
# translate_labels(labels = extract_labels(folder = "R"), target_language = "de")
# # albanais
# translate_labels(labels = extract_labels(folder = "R"), target_language = "sq")
# # polonais
# translate_labels(labels = extract_labels(folder = "R"), target_language = "pl")
# # portugais
# translate_labels(labels = extract_labels(folder = "R"), target_language = "pt")
# # turc
# translate_labels(labels = extract_labels(folder = "R"), target_language = "tr")
# # macédonien
# translate_labels(labels = extract_labels(folder = "R"), target_language = "mk", encoding = ?) # revoir encoding
# # japonais
# translate_labels(labels = extract_labels(folder = "R"), target_language = "ja", encoding = "ISO_2022,locale=ja,version=4") 
# # chinois
# translate_labels(labels = extract_labels(folder = "R"), target_language = "zh-CN", encoding = "chinese") # revoir encoding : "GBK", "UTF16_BigEndian", "GB18030"
# # coréen
# translate_labels(labels = extract_labels(folder = "R"), target_language = "ko", encoding = "csKOI8R") # "GBK", "korean"

