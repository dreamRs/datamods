
library(data.table)
library(stringr)
library(utils)
library(polyglotr)

source("inst/i18n/extract_labels.R")


# Extraction des labels contenus dans les sous-répertoires "/R" et "/examples"

extraction_labels <- c(extract_labels(folder = "R"), extract_labels(folder = "examples"))


# Mise à jour des csv avec les labels extraits précédemment SANS traduction

update_csv(labels = extraction_labels)


# Mise à jour des csv avec les labels extraits précédemment AVEC traduction

update_csv(labels = extraction_labels, translation = TRUE)


update_csv(labels = extraction_labels, lang = "fr", ....)
update_csv(labels = extraction_labels, lang = "es", ....)
update_csv(labels = extraction_labels, lang = "ja", encoding = "ISO_2022,locale=ja,version=4")
update_csv(labels = extraction_labels, lang = "fr", ....)
