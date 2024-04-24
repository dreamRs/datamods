

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
        pattern = "(?<=i18n..).+(?=\")"
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
  unique(extract_labels)
}


