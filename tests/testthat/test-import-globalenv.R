test_that("import_globalenv_ui works", {
  expect_is(import_globalenv_ui("ID"), "shiny.tag")
})
