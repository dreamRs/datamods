test_that("import_api_ui works", {
  expect_is(import_api_ui("ID"), "shiny.tag")
})

test_that("import_api_server works", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  shiny::testServer(import_api_server, {
    session$setInputs(link = 0) #to bypass ignoreInit = TRUE
    session$setInputs(
      link = "https://cdn.jsdelivr.net/gh/timruffles/gapminder-data-json@74aee1c2878e92608a6219c27986e7cd96154482/gapminder.min.json",
      confirm = 0
    )
    expect_is(imported_rv$data, "data.frame")
    expect_is(session$getReturned()$data(), "data.frame")
  })
})

