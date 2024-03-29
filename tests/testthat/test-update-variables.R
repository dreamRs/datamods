test_that("update_variables_ui works", {
  expect_is(update_variables_ui("ID"), "shiny.tag")
})


# test_that("update_variables_server works", {
#   data(mtcars)
#   shiny::testServer(update_variables_server, args = list(data = mtcars), {
#     tok <- token$x
#     selection <- as.list(rep(TRUE, ncol(mtcars)))
#     names(selection) <- paste("selection", tok, pad0(seq_along(mtcars)), sep = "-")
#
#     name <- as.list(names(mtcars))
#     names(name) <- paste("name", tok, pad0(seq_along(mtcars)), sep = "-")
#
#     class_toset <- as.list(rep("numeric", ncol(mtcars)))
#     names(class_toset) <- paste("class_to_set", tok, pad0(seq_along(mtcars)), sep = "-")
#
#     do.call(session$setInputs, selection)
#     do.call(session$setInputs, name)
#     do.call(session$setInputs, class_toset)
#
#     session$setInputs(validate = 1)
#
#     # print(session$getReturned()())
#     expect_is(session$getReturned()(), "data.frame")
#     expect_equal(session$getReturned()(), mtcars)
#   })
# })



test_that("get_classes works", {
  expect_is(get_classes(mtcars), "character")
  expect_length(get_classes(mtcars), ncol(mtcars))
})

test_that("get_classes works", {
  expect_is(get_n_unique(mtcars), "integer")
  expect_length(get_n_unique(mtcars), ncol(mtcars))
})

test_that("pad0 works", {
  expect_is(pad0(c(1, 15, 150, NA)), "character")
  expect_length(pad0(c(1, 15, 150, NA)), 4)
})

test_that("summary_vars works", {
  expect_is(summary_vars(mtcars), "data.frame")
  expect_identical(nrow(summary_vars(mtcars)), ncol(mtcars))
})



test_that("update_variables_datagrid works", {
  variables <- summary_vars(iris)
  dt <- update_variables_datagrid(variables)
  expect_is(dt, "htmlwidget")
})



test_that("convert_to works", {
  dat <- data.frame(
    v1 = month.name,
    v2 = month.abb,
    v3 = 1:12,
    v4 = as.numeric(Sys.Date() + 0:11),
    v5 = as.character(Sys.Date() + 0:11),
    v6 = as.factor(c("a", "a", "b", "a", "b", "a", "a", "b", "a", "b", "b", "a")),
    v7 = as.character(11:22),
    stringsAsFactors = FALSE
  )

  expect_is(
    convert_to(dat, "v3", "character")$v3,
    "character"
  )
  expect_is(
    convert_to(dat, "v6", "character")$v6,
    "character"
  )
  expect_is(
    convert_to(dat, "v7", "numeric")$v7,
    "numeric"
  )
  expect_is(
    convert_to(dat, "v4", "date", origin = "1970-01-01")$v4,
    "Date"
  )
  expect_is(
    convert_to(dat, "v5", "date")$v5,
    "Date"
  )
})



test_that("get_vars_to_convert works", {
  # 2 variables to convert
  new_classes <- list(
    "Sepal.Length" = "numeric",
    "Sepal.Width" = "numeric",
    "Petal.Length" = "character",
    "Petal.Width" = "numeric",
    "Species" = "character"
  )
  res <- get_vars_to_convert(summary_vars(iris), new_classes)
  expect_is(res, "data.frame")
  expect_identical(nrow(res), 2L)

  # No changes
  new_classes <- list(
    "Sepal.Length" = "numeric",
    "Sepal.Width" = "numeric",
    "Petal.Length" = "numeric",
    "Petal.Width" = "numeric",
    "Species" = "factor"
  )
  res <- get_vars_to_convert(summary_vars(iris), new_classes)
  expect_is(res, "data.frame")
  expect_identical(nrow(res), 0L)


  new_classes <- list(
    "mpg" = "character",
    "cyl" = "numeric",
    "disp" = "character",
    "hp" = "numeric",
    "drat" = "character",
    "wt" = "character",
    "qsec" = "numeric",
    "vs" = "character",
    "am" = "numeric",
    "gear" = "character",
    "carb" = "integer"
  )
  res <- get_vars_to_convert(summary_vars(mtcars), new_classes)
  expect_is(res, "data.frame")
  expect_identical(nrow(res), 7L)
})


