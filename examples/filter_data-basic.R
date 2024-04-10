library(shiny)
library(datamods)


ui <- fluidPage(
  tags$h2(i18n("Filter data.frame")),
  fluidRow(
    column(
      width = 3,
      filter_data_ui("filtering", max_height = "500px")
    ),
    column(
      width = 9,
      reactable::reactableOutput(outputId = "table"),
      tags$b(i18n("Code dplyr:")),
      verbatimTextOutput(outputId = "code_dplyr"),
      tags$b(i18n("Expression:")),
      verbatimTextOutput(outputId = "code"),
      tags$b(i18n("Filtered data:")),
      verbatimTextOutput(outputId = "res_str")
    )
  )
)

server <- function(input, output, session) {

  res_filter <- filter_data_server(
    id = "filtering",
    data = reactive(data.frame(
      varchar = month.name,
      varnum = 1:12,
      vardate = Sys.Date() + 1:12
    )),
    vars = reactive(list(
      "Variable character" = "varchar",
      "Variable date" = "vardate",
      "Variable numeric" = "varnum"
    )),
    drop_ids = FALSE
  )

  output$table <- reactable::renderReactable({
    reactable::reactable(res_filter$filtered(), pagination = FALSE)
  })

  output$code_dplyr <- renderPrint({
    res_filter$code()
  })
  output$code <- renderPrint({
    res_filter$expr()
  })

  output$res_str <- renderPrint({
    str(res_filter$filtered())
  })

}

if (interactive())
  shinyApp(ui, server)
