test_that("numeric value required for limit", {
  expect_error(
    get_timetable_metadata(api_key = Sys.getenv("BODS_KEY"),
                           limit = "25"),
    "Please provide an integer value to the limit argument")
})

test_that("helpful error message with bad authentication", {
  expect_error(
    get_timetable_metadata(api_key = "wrong key"),
    "Authentication credentials are not valid; please check you are using a valid BODS API key")
})
