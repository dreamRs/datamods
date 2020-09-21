test_that("import_googlesheets_ui works", {
  expect_is(import_googlesheets_ui("ID"), "shiny.tag")
})
