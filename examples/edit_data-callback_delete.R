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
    callback_delete = function(data, row) {
      print(data)
      print(row)
      if (row$Month == "September") {
        shinybusy::notify_warning(i18n("You cannot delete September"))
        return(FALSE)
      }
      return(TRUE)
    }
  )

  output$result <- renderPrint({
    str(edited_r())
  })

}

if (interactive())
  shinyApp(ui, server)
