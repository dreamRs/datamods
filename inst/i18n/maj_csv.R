
library(data.table)
library(stringr)
library(utils)
library(polyglotr)

source("inst/i18n/extract_labels.R")


# Extraction of labels contained in the “/R” subdirectory

extraction_labels <- c(extract_labels(folder = "R"))


# Updating csv with previously extracted labels WITHOUT translation

update_csv(labels = extraction_labels, lang = "fr", translation = FALSE)
update_csv(labels = extraction_labels, lang = "es", translation = FALSE)
update_csv(labels = extraction_labels, lang = "de", translation = FALSE)
update_csv(labels = extraction_labels, lang = "sq", translation = FALSE)
update_csv(labels = extraction_labels, lang = "pl", translation = FALSE)
update_csv(labels = extraction_labels, lang = "pt", translation = FALSE)
update_csv(labels = extraction_labels, lang = "tr", translation = FALSE)
update_csv(labels = extraction_labels, lang = "mk", translation = FALSE)
# update_csv(labels = extraction_labels, lang = "ja", translation = FALSE, encoding = "ISO_2022,locale=ja,version=4")  # review encoding
update_csv(labels = extraction_labels, lang = "zh-CN", translation = FALSE, encoding = "GBK") #"UTF16_BigEndian" or "GB18030"
update_csv(labels = extraction_labels, lang = "ko", translation = FALSE, encoding = "korean") # "GBK", "korean"


# Updating csv with labels extracted previously WITH translation

update_csv(labels = extraction_labels, lang = "fr")
update_csv(labels = extraction_labels, lang = "es")
update_csv(labels = extraction_labels, lang = "de")
update_csv(labels = extraction_labels, lang = "sq")
update_csv(labels = extraction_labels, lang = "pl")
update_csv(labels = extraction_labels, lang = "pt")
update_csv(labels = extraction_labels, lang = "tr")
update_csv(labels = extraction_labels, lang = "mk")
# update_csv(labels = extraction_labels, lang = "ja", encoding = "ISO_2022,locale=ja,version=4")  # review encoding
update_csv(labels = extraction_labels, lang = "zh-CN", encoding = "GBK") #"UTF16_BigEndian" or "GB18030"
update_csv(labels = extraction_labels, lang = "ko", encoding = "korean") # "GBK", "korean"
