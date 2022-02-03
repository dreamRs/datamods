#' @return
#' * UI: HTML tags that can be included in shiny's UI
#' * Server: a `list` with three slots:
#'   + **status**: a `reactive` function returning the status: `NULL`, `error` or `success`.
#'   + **name**: a `reactive` function returning the name of the imported data as `character`.
#'   + **data**: a `reactive` function returning the imported `data.frame`.
