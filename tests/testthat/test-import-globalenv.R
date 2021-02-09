test_that("import_globalenv_ui works", {
  expect_is(import_globalenv_ui("ID"), "shiny.tag")
})

test_that("import_globalenv_server works", {
  data(mtcars)
  shiny::testServer(import_globalenv_server, {
    session$setInputs(data = 0) #to bypass ignoreInit = TRUE
    session$setInputs(
      data = "mtcars",
      confirm = 0
    )
    expect_equal(imported_rv$name, input$data)
    expect_equal(session$getReturned()$name(), input$data)
    expect_is(imported_rv$data, "data.frame")
    expect_is(session$getReturned()$data(), "data.frame")

    session$setInputs(env = "datasets")
    session$setInputs(data = "faithful", confirm = 1)
    expect_is(session$getReturned()$data(), "data.frame")
    expect_equivalent(session$getReturned()$data(), faithful)
  })
})


test_that("get_dimensions works", {
  expect_null(get_dimensions(NULL))

  mydata <- mtcars
  mydata2 <- mtcars
  expect_is(get_dimensions("mydata"), "character")
  expect_length(get_dimensions(c("mydata", "mydata2")), 2)

  mylist <- list(a = 1)
  expect_identical(unname(get_dimensions("mylist")), "Not a data.frame")
})


test_that("list_pkg_data works", {
  expect_null(list_pkg_data("not.a.package"))
  expect_is(list_pkg_data("datasets"), "character")
})

