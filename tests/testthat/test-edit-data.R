
test_that("edit_data_ui works", {
  expect_is(edit_data_ui("ID"), "shiny.tag.list")
})


test_that("table_display works", {
  mydata <- iris
  mydata <- as.data.table(mydata)

  mydata[, .datamods_edit_update := as.character(seq_len(.N))]
  mydata[, .datamods_edit_delete := as.character(seq_len(.N))]
  mydata[, .datamods_id := seq_len(.N)]
  mydata <- table_display(mydata, colnames = NULL)

  expect_is(mydata, "reactable")
  expect_is(mydata, "htmlwidget")
  expect_length(mydata$x$tag$attribs$columns, 8)
  expect_equal(length(mydata$x$tag$attribs$columns), 8)
})


test_that("col_def_update works", {
  col_def_update <- col_def_update()
  expect_is(col_def_update, "colDef")
  expect_equal(col_def_update$name, "Update")
  expect_named(col_def_update, c('name', 'sortable', 'filterable', 'html', 'width'))
})


test_that("col_def_delete works", {
  col_def_delete <- col_def_delete()
  expect_is(col_def_delete, "colDef")
  expect_equal(col_def_delete$name, "Delete")
  expect_named(col_def_delete, c('name', 'sortable', 'filterable', 'html', 'width'))
})


test_that("btn_update works", {
  expect_is(btn_update("input"), "function")
  expect_is(btn_update("input")(1), "html")
  expect_is(btn_update("input")(1), "character")
})


test_that("btn_delete works", {
  expect_is(btn_delete("input"), "function")
  expect_is(btn_delete("input")(1), "html")
  expect_is(btn_delete("input")(1), "character")
})


test_that("confirmation_window works", {
  expect_is(confirmation_window(inputId = "input", title = "titre"), "shiny.tag")
})




