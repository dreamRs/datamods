library(shiny)
library(datamods)
library(bslib)
library(reactable)

ui <- fluidPage(
  theme = bs_theme(
    version = 5
  ),
  edit_data_ui(id = "id"),
  verbatimTextOutput("result")
)


server <- function(input, output, session) {

  edited_r <- edit_data_server(
    id = "id",
    data_r = reactive(data.frame(Month = month.name, Values = 1:12)),
    add = TRUE,
    callback_update = function(data, row) {
      print(data)
      print(row)
      if (!row$Month %in% month.name)
        return(FALSE)
      if (row$Values > 20) {
        shinybusy::notify_warning("Value must be equal or inferior to 20")
        return(FALSE)
      }
      # else update data
      return(TRUE)
    }
  )

  output$result <- renderPrint({
    str(edited_r())
  })

}

if (interactive())
  shinyApp(ui, server)
