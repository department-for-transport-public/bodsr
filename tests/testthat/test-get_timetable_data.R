test_that("returns expected message", {
  meta <- get_timetable_metadata(limit = 1)

  expect_message(get_timetable_data(meta))
})

test_that("data has expected size and shape", {
  meta <- get_timetable_metadata(limit = 5)

  expect_length(get_timetable_data(meta), 5)
  expect_length(get_timetable_data(meta)[1], 5)
  expect_equal(nrow(get_timetable_data(meta)[1]), 400)

})
