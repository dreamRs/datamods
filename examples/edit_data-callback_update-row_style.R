library(shiny)
# library(datamods)
pkgload::load_all()
library(bslib)
library(reactable)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    preset = "bootstrap"
  ),
  uiOutput(outputId = "custom_styles"),
  edit_data_ui(id = "id"),
  verbatimTextOutput("row_updated")
)


server <- function(input, output, session) {

  rv <- reactiveValues(row_updated = NULL)

  edited_r <- edit_data_server(
    id = "id",
    data_r = reactive(data.frame(
      row_id = 1:12,
      Month = month.name,
      Values = 1:12,
      Comment = letters[1:12]
    )),
    add = TRUE,
    var_edit = c("Month", "Values", "Comment"),
    reactable_options = list(
      pagination = FALSE,
      compact = TRUE,
      columns = list(row_id = colDef(show = FALSE)),
      rowClass = function(index) {
        paste0("table-row-", index)
      }
    ),
    callback_add = function(data, row) {
      print(row)
      return(TRUE)
    },
    callback_delete = function(data, row) {
      print(row)
      return(TRUE)
    },
    callback_update = function(data, row) {
      print(row)
      rv$row_updated <- row$row_id
      return(TRUE)
    }
  )

  output$row_updated <- renderPrint({
    paste("Last row updated:", rv$row_updated)
  })

  output$custom_styles <- renderUI({
    req(rv$row_updated)
    tags$style(sprintf(
      ".table-row-%s { background: #FE2E2E; transition: background 1s cubic-bezier(0.785, 0.135, 0.15, 0.86); color: white; }",
      rv$row_updated
    ))
  })

}

if (interactive())
  shinyApp(ui, server)
