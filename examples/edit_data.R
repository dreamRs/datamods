library(shiny)
library(datamods)
library(bslib)
library(reactable)

ui <- fluidPage(
  theme = bs_theme(
    version = 5
  ),
  tags$h2("Edit data", align = "center"),
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
    # var_edit = c("name", "job", "credit_card_provider", "credit_card_security_code"),
    var_mandatory = c("name", "job"),
    var_labels = list(
      name = "Name",
      credit_card_security_code = "Credit card security code",
      date_obtained = "Date obtained",
      contactless_card = "Contactless Card",
      credit_card_provider = "Credit card provider"
    ),
    add_default_values = list(
      name = "Please enter your name here",
      date_obtained = Sys.Date()
    ),
    n_column = 2,
    modal_size = "l",
    modal_easy_close = TRUE,
    reactable_options = list(
      defaultColDef = colDef(filterable = TRUE),
      selection = "single",
      columns = list(
        name = colDef(name = "Name", style = list(fontWeight = "bold")),
        credit_card_security_code = colDef(name = "Credit card security code"),
        date_obtained = colDef(name = "Date obtained", format = colFormat(date = TRUE)),
        contactless_card = colDef(
          name = "Contactless Card",
          cell = htmlwidgets::JS(
            "function(cellInfo) {
              return cellInfo.value ? '\u2714\ufe0f Yes' : '\u274c No';
            }"
          )
        ),
        credit_card_provider = colDef(
          name = "Credit card provider",
          style = htmlwidgets::JS(
            "function(rowInfo) {
              console.log(rowInfo);
              var value = rowInfo.values['credit_card_provider'];
              var color;
              if (value == 'Mastercard') {
                color = '#e06631';
              } else if (value == 'VISA 16 digit') {
                color = '#0c13cf';
              } else if (value == 'American Express') {
                color = '#4d8be8';
              } else if (value == 'JCB 16 digit') {
                color = '#23c45e';
              } else {
                color = '#777'
              }
              return {color: color, fontWeight: 'bold'}
            }"
          )
        )
      ),
      bordered = TRUE,
      compact = TRUE,
      searchable = TRUE,
      highlight = TRUE
    )
  )

  output$result <- renderPrint({
    str(edited_r())
  })

}

if (interactive())
  shinyApp(ui, server)
