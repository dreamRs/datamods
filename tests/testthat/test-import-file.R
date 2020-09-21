test_that("import_file_ui works", {
  expect_is(import_file_ui("ID"), "shiny.tag")
})
