test_that("onLoad works", {
  .onLoad()
  x <- shiny::resourcePaths()
  expect_true("datamods" %in% names(x))
})
