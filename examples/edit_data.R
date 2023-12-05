library(shiny)
library(datamods)
library(bslib)
library(reactable)

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
    # var_edit = c("name", "job", "credit_card_provider", "credit_card_security_code"),
    var_mandatory = c("name", "job"),
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
          cell = function(value) {
            # Render as an X mark or check mark
            if (value == FALSE) "\u274c No" else "\u2714\ufe0f Yes"
        }),
        credit_card_provider = colDef(
          name = "Credit card provider",
          style = function(value) {
            if (value == "Mastercard") {
              color <- "#e06631"
            } else if (value == "VISA 16 digit") {
              color <- "#0c13cf"
            } else if (value == "American Express") {
              color <- "#4d8be8"
            } else if (value == "JCB 16 digit") {
              color <- "#23c45e"
            } else {
              color <- "#777"
            }
            list(color = color, fontWeight = "bold")
          }
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
