test_that("import_copypaste_ui works", {
  expect_is(import_copypaste_ui("ID"), "shiny.tag")
})

test_that("import_copypaste_server works", {
  shiny::testServer(import_copypaste_server, {
    session$setInputs(data_pasted = 0) #to bypass ignoreInit = TRUE
    session$setInputs(
      data_pasted =
      "x y z
      1 2 3",
      confirm = 0
    )
    expect_is(imported_rv$data, "data.frame")
    expect_is(session$getReturned()$data(), "data.frame")
  })
})
