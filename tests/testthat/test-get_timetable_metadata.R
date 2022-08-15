test_that("numeric value required for limit", {
  expect_error(
    get_timetable_metadata(api_key = Sys.getenv("BODS_KEY"),
                           limit = "25"),
    "Please provide an integer value to the limit argument")
})
