test_that("validation_ui works", {
  expect_is(validation_ui("ID"), "shiny.tag.list")
  expect_is(validation_ui("ID", display = "inline"), "shiny.tag.list")
})

# test_that("validation_server works", {
#   rv <- shiny::reactiveVal(cars)
#   shiny::testServer(
#     validation_server,
#     args = list(
#       data = rv,
#       n_row = ~ . > 20,
#       n_col = ~ . >= 3,
#       rules = validate::validator(
#         speed >= 0
#         , dist >= 0
#         , speed/dist <= 1.5
#       )
#     ), {
#       rv(cars)
#       expect_is(valid_ui$x, "shiny.tag")
#       expect_is(valid_rv$status, "character")
#     }
#   )
# })
