
#' @importFrom htmltools tagList tags css
describe_col_char <- function(x, with_summary = TRUE) {
  tags$div(
    style = css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = css(fontStyle = "italic"),
      phosphoricons::ph("text-aa"),
      "character"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "3px 0")),
        tags$div(
          i18n("Unique:"), length(unique(x))
        ),
        tags$div(
          i18n("Missing:"), sum(is.na(x))
        ),
        tags$div(
          style = css(whiteSpace = "normal", wordBreak = "break-all"),
          i18n("Most Common:"), gsub(
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

fmt_p <- function(val, tot) {
  paste0(round(val / tot * 100, 1), "%")
}

describe_col_factor <- function(x, with_summary = TRUE) {
  count <- sort(table(x, useNA = "always"), decreasing = TRUE)
  total <- sum(count)
  one <- count[!is.na(names(count))][1]
  two <- count[!is.na(names(count))][2]
  missing <- count[is.na(names(count))]
  tags$div(
    style = css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = css(fontStyle = "italic"),
      phosphoricons::ph("list-bullets"),
      "factor"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "3px 0")),
        tags$div(
          names(one), ":", fmt_p(one, total)
        ),
        tags$div(
          names(two), ":", fmt_p(two, total)
        ),
        tags$div(
          "Missing", ":", fmt_p(missing, total)
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
    style = css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = css(fontStyle = "italic"),
      phosphoricons::ph("hash"),
      "numeric"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "3px 0")),
        tags$div(
          i18n("Min:"), round(min(x, na.rm = TRUE), 2)
        ),
        tags$div(
          i18n("Mean:"), round(mean(x, na.rm = TRUE), 2)
        ),
        tags$div(
          i18n("Max:"), round(max(x, na.rm = TRUE), 2)
        ),
        tags$div(
          i18n("Missing:"), sum(is.na(x))
        )
      )
    }
  )
}


describe_col_date <- function(x, with_summary = TRUE) {
  tags$div(
    style = css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = css(fontStyle = "italic"),
      phosphoricons::ph("calendar"),
      "date"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "3px 0")),
        tags$div(
          i18n("Min:"), min(x, na.rm = TRUE)
        ),
        tags$div(
          i18n("Max:"), max(x, na.rm = TRUE)
        ),
        tags$div(
          i18n("Missing:"), sum(is.na(x))
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
    style = css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = css(fontStyle = "italic"),
      phosphoricons::ph("clock"),
      "datetime"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "3px 0")),
        tags$div(
          i18n("Min:"), min(x, na.rm = TRUE)
        ),
        tags$div(
          i18n("Max:"), max(x, na.rm = TRUE)
        ),
        tags$div(
          i18n("Missing:"), sum(is.na(x))
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
    style = css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = css(fontStyle = "italic"),
      # phosphoricons::ph("clock"),
      paste(class(x), collapse = ", ")
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = css(margin = "3px 0")),
        tags$div(
          i18n("Unique:"), length(unique(x))
        ),
        tags$div(
          i18n("Missing:"), sum(is.na(x))
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
    height = 90,
    columnContent = lapply(
      X = setNames(names(data), names(data)),
      FUN = function(col) {
        values <- data[[col]]
        content <- if (inherits(values, "character")) {
          describe_col_char(values)
        } else if (inherits(values, "factor")) {
          describe_col_factor(values)
        } else if (inherits(values, c("numeric", "integer"))) {
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
