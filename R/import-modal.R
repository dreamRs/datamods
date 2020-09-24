

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
import_ui <- function(id, from) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("tabs"),
    selected = from,
    tabPanel(
      title = "From Environment",
      value = "env",
      import_globalenv_ui(ns("env"))
    ),
    tabPanel(
      title = "From File",
      value = "file",
      import_file_ui(ns("file"))
    ),
    tabPanel(
      title = "From Copy/Paste",
      value = "copypaste",
      import_copypaste_ui(ns("copypaste"))
    ),
    tabPanel(
      title = "From Googlesheets",
      value = "googlesheets",
      import_googlesheets_ui(ns("googlesheets"))
    ),
    tabPanel(
      title = "From Database",
      value = "database"
    )
  )
}


#' @export
#' 
#' @importFrom shiny moduleServer reactiveValues observeEvent reactive
#'
#' @rdname import-modal
import_server <- function(id,
                          from = c("env", "file", "copypaste", "googlesheets", "database")) {
  
  from = match.arg(from)
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      import_modal(id, from)

      server_function <- reactiveValues(server = NULL)
      
      observeEvent(input$tabs, {
        from = input$tabs
        server_selected <- switch(
          from,
          "env" = import_globalenv_server,
          "file" = import_file_server,
          "copypaste" = import_copypaste_server,
          "googlesheets" = import_googlesheets_server
        )
        server_function$server <- server_selected(from)
      })
      
      return(list(
        data = reactive(server_function$server$data())
      ))
    }
  )
}


# utils -------------------------------------------------------------------

#' @importFrom shiny modalDialog showModal
import_modal <- function(id, from) {
  showModal(modalDialog(import_ui(id, from)))
}