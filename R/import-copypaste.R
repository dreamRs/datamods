
#' import-copypaste UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList textAreaInput
#' @importFrom data.table fread
mod_import_copypaste_ui <- function(id){
  ns <- NS(id)
  tagList(
    html_dependency_datamods(),
    textAreaInput(
      inputId = ns("data_pasted"),
      label = "Paste Code",
      height = "500px"
    )
  )
}

#' import-copypaste Server Function
#'
#' @noRd 
mod_import_copypaste_server <- function(input, output, session){
  ns <- session$ns
  
  d1 <- reactive({
    req(input$data_pasted)
    data.table::fread(input = input$data_pasted)
  })
  return(
    list(
      data = d1
    )
  )
}

## To be copied in the UI
# 

## To be copied in the server
# 

