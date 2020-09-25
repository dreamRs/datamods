

#' @title Get all import functions in a Modal
#' 
#' @description Let the user choose a method of his choice to import data 
#'
#' @param id Module's id
#' @param from The import_ui & server to use, i.e. the method.
#'   There are 5 options to choose from. ("env", "file", "copypaste", "googlsheets" & "database")
#' 
#' @return
#'  * UI: HTML tags that can be included in shiny's UI
#'  * Server: a \code{list} with one slot:
#'    + **data**: a \code{reactive} function returning the selected \code{data.frame}.
#'    
#' @export
#' @name import-modal
#' 
#' @importFrom shiny NS tabsetPanel tabPanel
#'
#' @example examples/modal.R
#' 
import_ui <- function(id, from = c("env", "file", "copypaste", "googlesheets", "database")) {
  ns <- NS(id)
  from <- match.arg(from, several.ok = TRUE)
  env <- if("env" %in% from) tabPanel("Env", import_globalenv_ui(ns("env")))
  file <- if("file" %in% from) tabPanel("File", import_file_ui(ns("file")))
  copypaste <- if("copypaste" %in% from) tabPanel("Copy/Paste", import_copypaste_ui(ns("copypaste")))
  googlesheets <- if("googlesheets" %in% from) tabPanel("Googlesheets", import_googlesheets_ui(ns("googlesheets")))
  #database <- if("database" %in% from) tabPanel("Database", import_database_ui(ns("database")))
  do.call(tabsetPanel, dropNulls(list(env, file, copypaste, googlesheets)))
}


#' @export
#' 
#' @importFrom shiny moduleServer reactiveValues observeEvent reactive
#'
#' @rdname import-modal
import_server <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session){
      
      data_rv <- reactiveValues(x = NULL)
      
      from_env <- import_globalenv_server("env")
      from_file <- import_file_server("file")
      from_copypaste <- import_copypaste_server("copypaste")
      from_googlesheets <- import_googlesheets_server("googlesheets")
      #from_database <- import_database_server("database")
      
      observeEvent(from_env$data(), {
        data_rv$x <- from_env$data()
      })
      observeEvent(from_file$data(), {
        data_rv$x <- from_file$data()
      })
      observeEvent(from_copypaste$data(), {
        data_rv$x <- from_copypaste$data()
      })
      observeEvent(from_googlesheets$data(), {
        data_rv$x <- from_googlesheets$data()
      })
      # observeEvent(from_database$data(), {
      #   data_rv$x <- from_database$data()
      # })
      
      return(reactive(data_rv$x))
    }
  )
}


# utils -------------------------------------------------------------------

#' @importFrom shiny modalDialog showModal
import_modal <- function(id, from) {
  showModal(modalDialog(import_ui(id, from)))
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}