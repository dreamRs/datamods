test_that("import_googlesheets_ui works", {
  expect_is(import_googlesheets_ui("ID"), "shiny.tag")
})

test_that("import_googlesheets_server works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  shiny::testServer(import_googlesheets_server, {
    session$setInputs(link = 0) #to bypass ignoreInit = TRUE
    session$setInputs(
      link = "https://docs.google.com/spreadsheets/d/1U6Cf_qEOhiR9AZqTqS3mbMF3zt2db48ZP5v3rkrAEJY/edit?usp=sharing",
      confirm = 0
    )
    expect_is(imported_rv$data, "data.frame")
    expect_is(session$getReturned()$data(), "data.frame")
  })
})

