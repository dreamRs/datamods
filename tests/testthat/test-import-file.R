test_that("import_file_ui works", {
  expect_is(import_file_ui("ID"), "shiny.tag")
})

test_that("import_file_server works", {
  shiny::testServer(import_file_server, {
    session$setInputs(sheet = 0) #to bypass ignoreInit = TRUE
    session$setInputs(
      file = data.frame(
        datapath = system.file("extdata", "mtcars.csv", package = "datamods"),
        stringsAsFactors = FALSE
      ),
      sheet = 0,
      skip_rows = 0,
      confirm = 0,
      dec = ".",
      encoding = "UTF-8",
      na_label = ",NA"
    )
    expect_is(imported_rv$data, "data.frame")
    expect_is(session$getReturned()$data(), "data.frame")
  })
})

