test_that("onLoad works", {
  .onLoad()
  x = resourcePaths()
  expect_true("datamods" %in% names(x))
})
