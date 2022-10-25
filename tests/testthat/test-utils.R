test_that("not_null pastes together objects if not null", {

  x = 2
  expect_equal(not_null(x, "number"), "number=2&")

})

test_that("not_null returns null correctly", {

  x = NULL
  expect_null(not_null(x, "number"))

})
