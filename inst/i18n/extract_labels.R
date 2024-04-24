#' Function to extract labels 
#'
#' @param folder file directory 
#'
#' @return an extraction of the labels contained in the directory files
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
