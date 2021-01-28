test_that("import_ui works", {
  expect_is(import_ui("ID"), "shiny.tag")
})

test_that("import_server works", {
  shiny::testServer(import_server, args = list(return_class = "data.table"), {
    data(mtcars)
    session$env$data_rv$data = mtcars
    session$env$data_rv$name = "mtcars"

    session$setInputs(confirm = 1)

    expect_is(session$getReturned()$data(), "data.table")
    expect_equal(session$getReturned()$name(), "mtcars")
  })
})
