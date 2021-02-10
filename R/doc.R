

doc_return_import <- function() {
  c(
    "@return",
    "* UI: HTML tags that can be included in shiny's UI",
    "* Server: a `list` with three slots:",
    "  + **status**: a `reactive` function returning the status: `NULL`, `error` or `success`.",
    "  + **name**: a `reactive` function returning the name of the imported data as `character`.",
    "  + **data**: a `reactive` function returning the imported `data.frame`."
  )
}

doc_return_filter <- function() {
  c(
    "@return",
    "* UI: HTML tags that can be included in shiny's UI",
    "* Server: a `list` with three slots:",
    "  + **filtered**: a `reactive` function returning the data filtered.",
    "  + **code**: a `reactive` function returning the dplyr pipeline to filter data.",
    "  + **expr**: a `reactive` function returning an expression to filter data."
  )
}
