
test_that("filter_data_ui works", {
  expect_is(filter_data_ui("ID"), "shiny.tag.list")
})


test_that("create_filters works", {

  filters <- create_filters(iris, session = list(ns = identity))

  expect_length(filters, 3)
  expect_named(filters, c("ui", "filters_id", "filters_na_id"))

  expect_is(filters$ui, "shiny.tag.list")

  expect_length(filters$filters_id, ncol(iris))
  expect_equal(length(filters$ui[[1]]), length(filters$filters_id))
  expect_equal(length(filters$filters_id), length(filters$filters_na_id))
})


test_that("create_filters with options works", {

  filters <- create_filters(iris, vars = names(iris)[1:3], widget_num = "range", session = list(ns = identity))

  expect_length(filters, 3)
  expect_named(filters, c("ui", "filters_id", "filters_na_id"))

  expect_is(filters$ui, "shiny.tag.list")

  expect_length(filters$filters_id, 3)
  expect_equal(length(filters$ui[[1]]), length(filters$filters_id))
  expect_equal(length(filters$filters_id), length(filters$filters_na_id))
})


test_that("create_filters with dates and ids works", {

  mydata <- data.frame(
    date = seq(as.Date("2021-01-01"), by = "1 month", length.out = 12),
    name = month.name,
    num = rep(c(1, 2), each = 6)
  )

  filters <- create_filters(mydata, session = list(ns = identity))

  expect_length(filters, 3)
  expect_named(filters, c("ui", "filters_id", "filters_na_id"))

  expect_is(filters$ui, "shiny.tag.list")

  expect_length(filters$filters_id, 2)
  expect_equal(length(filters$ui[[1]]), length(filters$filters_id))
  expect_equal(length(filters$filters_id), length(filters$filters_na_id))
})


test_that("create_filters with dates and ids works (bis)", {

  mydata <- data.frame(
    date = seq(as.Date("2021-01-01"), by = "1 month", length.out = 12),
    name = month.name,
    num = rep(c(1, 2), each = 6)
  )

  filters <- create_filters(mydata, widget_date = "range", session = list(ns = identity))

  expect_length(filters, 3)
  expect_named(filters, c("ui", "filters_id", "filters_na_id"))

  expect_is(filters$ui, "shiny.tag.list")

  expect_length(filters$filters_id, 2)
  expect_equal(length(filters$ui[[1]]), length(filters$filters_id))
  expect_equal(length(filters$filters_id), length(filters$filters_na_id))
})


test_that("make_expr_filter works", {

  filter_inputs <- lapply(
    X = iris,
    FUN = function(x) {
      sort(sample(unique(x), 2))
    }
  )
  filter_nas <- lapply(
    X = iris,
    FUN = function(x) {
      sample(c(TRUE, FALSE), 2)
    }
  )

  filters <- make_expr_filter(
    filters = filter_inputs,
    filters_na = filter_nas,
    data = iris,
    data_name = "iris"
  )

  expect_length(filters, 2)
  expect_named(filters, c("expr_dplyr", "expr"))

  expect_is(filters$expr_dplyr, "call")
  expect_is(filters$expr, "call")
})


test_that("make_expr_filter with dates works", {

  mydata <- data.frame(
    date = seq(as.Date("2021-01-01"), by = "1 month", length.out = 12),
    name = month.name,
    num = rep(c(1, 2), each = 6)
  )

  filter_inputs <- lapply(
    X = mydata,
    FUN = function(x) {
      sort(sample(unique(x), 2))
    }
  )
  filter_nas <- lapply(
    X = mydata,
    FUN = function(x) {
      sample(c(TRUE, FALSE), 2)
    }
  )

  filters <- make_expr_filter(
    filters = filter_inputs,
    filters_na = filter_nas,
    data = mydata,
    data_name = "mydata"
  )

  expect_length(filters, 2)
  expect_named(filters, c("expr_dplyr", "expr"))

  expect_is(filters$expr_dplyr, "call")
  expect_is(filters$expr, "call")
})
