test_that("i18n works if option not set", {
  options("datamods.i18n" = NULL)
  label <- "something"
  expect_identical(i18n(label), label)
})


test_that("i18n works with translation argument", {
  options("datamods.i18n" = NULL)
  label <- "something"
  translation <- "quelque chose"
  l <- list(translation)
  names(l) <- label
  expect_identical(i18n(label, l), translation)
})


test_that("i18n works with option set", {
  options("datamods.i18n" = NULL)
  label <- "something"
  translation <- "quelque chose"
  l <- list(translation)
  names(l) <- label
  options("i18n" = l)
  expect_identical(i18n(label, i18n_translations(NULL)), translation)
})


test_that("i18n works with list", {
  label <- "something"
  translation <- "quelque chose"
  l <- list(translation)
  names(l) <- label
  options("datamods.i18n" = l)
  expect_identical(i18n_test(label), translation)
  expect_warning(i18n_test("label"))
})


test_that("i18n works with data.frame", {
  label <- "something"
  translation <- "quelque chose"
  options("datamods.i18n" = data.frame(
    label = label,
    translation = translation,
    stringsAsFactors = FALSE
  ))
  expect_identical(i18n_test(label), translation)
  expect_warning(i18n_test("label"))
})


test_that("i18n works with file", {
  options("datamods.i18n" = system.file(
    "i18n", "fr.csv", package = "datamods"
  ))
  expect_identical(i18n_test("Help"), "Aide")
})


test_that("i18n works with supported language", {
  options("datamods.i18n" = "fr")
  expect_identical(i18n_test("Help"), "Aide")

  options("datamods.i18n" = "mk")
  expect_type(i18n_test("Help"), "character")

  options("datamods.i18n" = "pt")
  expect_type(i18n_test("Help"), "character")

  options("datamods.i18n" = "al")
  expect_type(i18n_test("Help"), "character")
})


test_that("i18n dont work if no list, no data.frame, no file", {
  options("datamods.i18n" = Sys.Date())
  on.exit(options("datamods.i18n" = NULL))
  label <- "something"
  expect_error(i18n_test(label))
})
