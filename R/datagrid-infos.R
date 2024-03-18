
#' @importFrom htmltools tagList tags css
describe_col_char <- function(x, with_summary = TRUE) {
  tags$div(
    style = css(padding = "5px 0", fontSize = "smaller"),
    tags$div(
      style = css(fontStyle = "italic"),
      phosphoricons::ph("text-aa"),
      "character"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "5px 0")),
        tags$div(
          "Unique:", length(unique(x))
        ),
        tags$div(
          "Missing:", sum(is.na(x))
        ),
        tags$div(
          style = css(whiteSpace = "normal", wordBreak = "break-all"),
          "Most Common:", gsub(
            pattern = "'",
            replacement = "\u07F4",
            x = names(sort(table(x), decreasing = TRUE))[1]
          )
        ),
        tags$div(
          "\u00A0"
        )
      )
    }
  )
}

describe_col_num <- function(x, with_summary = TRUE) {
  tags$div(
    style = css(padding = "5px 0", fontSize = "smaller"),
    tags$div(
      style = css(fontStyle = "italic"),
      phosphoricons::ph("hash"),
      "numeric"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "5px 0")),
        tags$div(
          "Min:", min(x, na.rm = TRUE)
        ),
        tags$div(
          "Mean:", mean(x, na.rm = TRUE)
        ),
        tags$div(
          "Max:", max(x, na.rm = TRUE)
        ),
        tags$div(
          "Missing:", sum(is.na(x))
        )
      )
    }
  )
}


describe_col_date <- function(x, with_summary = TRUE) {
  tags$div(
    style = css(padding = "5px 0", fontSize = "smaller"),
    tags$div(
      style = css(fontStyle = "italic"),
      phosphoricons::ph("calendar"),
      "date"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "5px 0")),
        tags$div(
          "Min:", min(x, na.rm = TRUE)
        ),
        tags$div(
          "Max:", max(x, na.rm = TRUE)
        ),
        tags$div(
          "Missing:", sum(is.na(x))
        ),
        tags$div(
          "\u00A0"
        )
      )
    }
  )
}

describe_col_datetime <- function(x, with_summary = TRUE) {
  tags$div(
    style = css(padding = "5px 0", fontSize = "smaller"),
    tags$div(
      style = css(fontStyle = "italic"),
      phosphoricons::ph("clock"),
      "datetime"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "5px 0")),
        tags$div(
          "Min:", min(x, na.rm = TRUE)
        ),
        tags$div(
          "Max:", max(x, na.rm = TRUE)
        ),
        tags$div(
          "Missing:", sum(is.na(x))
        ),
        tags$div(
          "\u00A0"
        )
      )
    }
  )
}


describe_col_other <- function(x, with_summary = TRUE) {
  tags$div(
    style = css(padding = "5px 0", fontSize = "smaller"),
    tags$div(
      style = css(fontStyle = "italic"),
      # phosphoricons::ph("clock"),
      paste(class(x), collapse = ", ")
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "5px 0")),
        tags$div(
          "Unique:", length(unique(x))
        ),
        tags$div(
          "Missing:", sum(is.na(x))
        ),
        tags$div(
          "\u00A0"
        ),
        tags$div(
          "\u00A0"
        )
      )
    }
  )
}

#' @importFrom htmltools doRenderTags
construct_col_summary <- function(data) {
  list(
    position = "top",
    height = 40,
    columnContent = lapply(
      X = setNames(names(data), names(data)),
      FUN = function(col) {
        values <- data[[col]]
        content <- if (inherits(values, c("character"," factor"))) {
          describe_col_char(values)
        } else if (inherits(values, c("numeric"," integer"))) {
          describe_col_num(values)
        } else if (inherits(values, c("Date"))) {
          describe_col_date(values)
        } else if (inherits(values, c("POSIXt"))) {
          describe_col_datetime(values)
        } else {
          describe_col_other(values)
        }
        list(
          template = toastui::JS(
            "function(value) {",
            sprintf(
              "return '%s';",
              gsub(replacement = "", pattern = "\n", x = doRenderTags(content))
            ),
            "}"
          )
        )
      }
    )
  )
}
