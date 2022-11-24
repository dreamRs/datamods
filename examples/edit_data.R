library(shiny)
library(datamods)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5
  ),
  tags$h2(i18n("Edit data"), align = "center"),
  edit_data_ui(id = "id"),
  verbatimTextOutput("result")
)


server <- function(input, output, session) {

  edited_r <- edit_data_server(
    id = "id",
    data_r = reactive(demo_edit),
    add = TRUE,
    update = TRUE,
    delete = TRUE,
    download_csv = TRUE,
    download_excel = TRUE,
    file_name_export = "datas",
    var_edit = c("name", "job", "credit_card_provider", "credit_card_security_code"),
    var_mandatory = c("name", "job")
  )

  output$result <- renderPrint({
    str(edited_r())
  })

}

if (interactive())
  shinyApp(ui, server)
