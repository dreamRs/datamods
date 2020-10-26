test_that("import_ui works", {
  expect_is(import_ui("ID"), "shiny.tag")
})

