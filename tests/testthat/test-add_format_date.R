# Tests for functions
test_that("add_format_date", {
  # Create an input data set
  input_data <- structure(list(id = 1:6, station = c("SSK_LH003", "SSK_LH003",
                                                     "SSK_LH003", "SSK_LH003", "SSK_LH003", "SSK_LH003"), parameter = c("pm10_kal",
                                                                                                                        "pm10_kal", "pm10_kal", "pm10_kal", "pm10_kal", "pm10_kal"),
                               value = c(13.275, 10.035, 12.04, 13.596, 12.834, 19.082),
                               aggregation = c(3600, 3600, 3600, 3600, 3600, 3600), timestamp = c(1643673600L,
                                                                                                  1643677200L, 1643680800L, 1643684400L, 1643688000L, 1643691600L
                               )), row.names = c(NA, 6L), class = "data.frame")

  # Format to timezone Europe/Amsterdam
  output_europe <- add_format_date(input_data, "Europe/Amsterdam")

  # Format to timezone UTC
  output_utc <- add_format_date(input_data, "UTC")

  # expect_equal(output_europe$date, c())
  expect_true("date" %in% names(output_europe))

})
