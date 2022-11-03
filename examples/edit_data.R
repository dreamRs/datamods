library(shiny)
library(datamods)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5
    ),
  tags$h2("Edit data", align = "center"),
  edit_data_ui("id"),
  verbatimTextOutput("result")
)


server <- function(input, output, session) {

  edited_r <- edit_data_server(
    "id",
    data_r = reactive(demo_mod_edit_data),
    add = TRUE,
    update = TRUE,
    delete = TRUE,
    download_csv = TRUE,
    download_excel = TRUE,
    file_name_export = "datas",
    var_edit = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Species"),
    var_mandatory = c("Sepal.Length", "Sepal.Width")
  )

  output$result <- renderPrint({
    str(edited_r())
  })
}

if (interactive())
  shinyApp(ui, server)
