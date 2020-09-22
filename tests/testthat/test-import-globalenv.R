test_that("import_globalenv_ui works", {
  expect_is(import_globalenv_ui("ID"), "shiny.tag")
})

test_that("import_globalenv_server works", {
  data(mtcars)
  shiny::testServer(import_globalenv_server, {
    session$setInputs(data = 0) #to bypass ignoreInit = TRUE
    session$setInputs(
      data = "mtcars",
      validate = 0
    )
    expect_equal(imported_rv$name, input$data)
    expect_equal(session$getReturned()$name(), input$data)
    expect_is(imported_rv$data, "data.frame")
    expect_is(session$getReturned()$data(), "data.frame")
  })
})

