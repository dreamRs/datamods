test_that("import_url_ui works", {
  expect_is(import_url_ui("ID"), "shiny.tag")
})

test_that("import_url_server works with json", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  shiny::testServer(import_url_server, {
    session$setInputs(link = 0) #to bypass ignoreInit = TRUE
    session$setInputs(
      link = "https://raw.githubusercontent.com/dreamRs/datamods/master/inst/extdata/mtcars.json",
      confirm = 0
    )
    expect_is(imported_rv$data, "data.frame")
    expect_is(session$getReturned()$data(), "data.frame")
  })
})

test_that("import_url_server works with csv", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  shiny::testServer(import_url_server, {
    session$setInputs(link = 0) #to bypass ignoreInit = TRUE
    session$setInputs(
      link = "https://raw.githubusercontent.com/dreamRs/datamods/master/inst/extdata/mtcars.csv",
      confirm = 0
    )
    expect_is(imported_rv$data, "data.frame")
    expect_is(session$getReturned()$data(), "data.frame")
  })
})

test_that("import_url_server works with shortened URL", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  shiny::testServer(import_url_server, {
    session$setInputs(link = 0) #to bypass ignoreInit = TRUE
    session$setInputs(
      link = "https://tinyurl.com/datamodsjson",
      confirm = 0
    )
    expect_is(imported_rv$data, "data.frame")
    expect_is(session$getReturned()$data(), "data.frame")
  })
})

