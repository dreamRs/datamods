library(tidyr)
# ?pivot_longer

library(data.table)
# ?melt

dt <- as.data.table(billboard)

melt(
  data = dt,
  id.vars = c("artist", "track", "date.entered"),
  measure.vars = NULL,
  na.rm = TRUE
)

melt(
  data = dt,
  id.vars = NULL,
  measure.vars = patterns("^wk"),
  na.rm = TRUE
)

billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )

billboard %>%
  pivot_longer(
    cols = !c(artist, track, date.entered),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )


anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)"
  )

melt(
  data = as.data.table(anscombe),
  measure.vars = names(anscombe)
)






# Construct call ----------------------------------------------------------

library(rlang)

call_pivot_longer <- function(data, to_keep = NULL, to_rows = NULL, variable_name = "variable", value_name = "value", na_rm = FALSE) {
  if (is.null(to_rows) & !is.null(to_keep)) {
    expr(
      !!sym(data) %>%
        pivot_longer(!c(!!!syms(to_keep)), names_to = !!variable_name, values_to = !!value_name, values_drop_na = !!na_rm)
    )
  } else if (!is.null(to_rows) & is.null(to_keep)) {
    expr(
      !!sym(data) %>%
        pivot_longer(c(!!!syms(to_rows)), names_to = !!variable_name, values_to = !!value_name, values_drop_na = !!na_rm)
    )
  } else if (!is.null(to_rows) & !is.null(to_keep)) {
    if (length(to_rows) < length(to_keep)) {
      call_pivot_longer(data, to_rows = to_rows, to_keep = NULL)
    } else {
      call_pivot_longer(data, to_rows = NULL, to_keep = to_keep)
    }
  } else {
    stop("You should provide to_keep or to_rows or both.")
  }
}

call_pivot_longer("billboard", to_keep = c("artist", "track", "date.entered"))
call_pivot_longer("billboard", to_rows = grep(pattern = "^wk", x = names(billboard), value = TRUE))
call_pivot_longer("billboard", to_keep = c("artist", "track", "date.entered"), to_rows = grep(pattern = "^wk", x = names(billboard), value = TRUE))
call_pivot_longer("billboard")


