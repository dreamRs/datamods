
library(stringr)
library(readr)
library(utils)
library(dplyr) 
library(polyglotr)

source("inst/i18n/extract_labels.R")


# Extraction des labels contenus dans les sous-répertoires "/R" et "/examples"

extraction_labels <- c(extract_labels(folder = "R"), extract_labels(folder = "examples"))


# Mise à jour des csv avec les labels extraits précédemment SANS traduction

update_csv(labels = extraction_labels) 


# Mise à jour des csv avec les labels extraits précédemment AVEC traduction

update_csv(labels = extraction_labels, translation = TRUE) 

