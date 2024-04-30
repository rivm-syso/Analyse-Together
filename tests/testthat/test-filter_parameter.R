# Tests for functions
test_that("filter_parameter", {
  # Create an input data set
  input_data <- data.frame(station = c("KNMI_260", "KNMI_260", "KNMI_260",
                                          "KNMI_260", "KNMI_260", "KNMI_260", "KNMI_260", "KNMI_260", "LTD_70904",
                                          "LTD_70904", "LTD_70904", "LTD_70904", "LTD_70904", "NL10617",
                                          "NL10617", "NL10617", "NL10617", "NL10636", "NL10636", "NL10636",
                                          "NL10636", "NL10636", "NL10636", "NL10636", "NL10636", "NL10617",
                                          "NL10636", "NL10636", "NL10636", "NL10636", "LTD_70904", "LTD_70904"
  ), parameter = c("wd", "ws", "temp", "rh", "wd", "ws", "temp",
                   "rh", "pres", "rh", "temp", "pm10_kal", "pm25_kal", "no", "no2",
                   "o3", "pm10", "no", "no2", "pm10", "pm25", "no", "no2", "pm10",
                   "pm25", "pm10_kal", "pm10_kal", "pm10_kal", "pm25_kal", "pm25_kal",
                   "pm10", "pm25"),
  value = c(120, 1, 12.6, 97, 150, 2, 12.7, 96,
            1012.83, 96.53, 13.82, 10.472, 5.184, 0.25, 8.15, 10.69, 5.71,
            1.39, 35.98, 17.23, 5.84, 0.95, 28.98, 12.11, 4.8, 5.71, 17.23,
            12.11, 5.84, 4.8, 5.59, 3.2))

  # Input parameter
  parameter_input <- "pm25_kal"

  # Expected outpur
  expect_output_data <- data.frame(station = c("KNMI_260", "KNMI_260", "KNMI_260",
                                                   "KNMI_260", "LTD_70904", "NL10636", "NL10636"),
                                   parameter = c("wd","ws", "wd", "ws", "pm25_kal", "pm25_kal", "pm25_kal"),
                                   value = c(120, 1, 150, 2, 5.184, 5.84, 4.8))

  # Filter on parameter
  output_data <- filter_parameter(input_data, parameter_input)

  # TEST results
  expect_equal(output_data, expect_output_data)

})
