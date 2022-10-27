library(shiny)
library(phosphoricons)
library(htmltools)
library(data.table)
library(reactable)
library(shinyWidgets)
library(dplyr)
library(rlang)
library(writexl)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5
    ),
  tags$h2("Edit data", align = "center"),
  edit_data_ui("id")
)


server <- function(input, output, session) {

  edit_data_server(
    "id",
    data_r = reactive(iris),
    add = TRUE,
    update = TRUE,
    delete = TRUE,
    download_csv = TRUE,
    download_excel = TRUE,
    file_name_export = "datas",
    var_edit = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Species"),
    var_mandatory = c("Sepal.Length", "Sepal.Width"))
}

if (interactive())
  shinyApp(ui, server)
