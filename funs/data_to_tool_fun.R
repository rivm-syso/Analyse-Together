# misc functions to get data from the database and in the format used in the tool

get_measurements <- function(measurements_con, stations_name, start_time, end_time){
  # Function to get all the measurements from the stations in the period
  # arguments:
  #    measurements_con:
  #    stations_name:
  #    start_time:
  #    end_time:
  #
  # return: dataframe

  # Get the data measurements of the selected Municipality/project
  data_measurements <- measurements_con %>%
    dplyr::filter(station %in% stations_name) %>%
    as.data.frame()

  # Filter the measurements on time
  data_measurements <- data_measurements %>%
    dplyr::mutate(date = lubridate::as_datetime(timestamp, tz = "Europe/Amsterdam")) %>%
    dplyr::filter(date > start_time &
                    date < end_time)

  return(data_measurements)
}

get_locations <- function(stations_con, stations_name){
  # Function to get the location and metadata from the stations
  # arguments:
  #    stations_con:
  #    stations_name:
  #
  # return: dataframe

  # Get the information from the stations
  data_locations <- stations_con %>%
    dplyr::filter(station %in% stations_name) %>%
    as.data.frame()

  return(data_locations)
}

add_ref_kal <- function(data_measurements){
  # Function to add a pm10_kal and pm25_kal value to the reference stations. In
  # the Netherlands the sensor do have a calibrated value, for the reference
  # stations the normal value is also set as calibrated value.
  # argument:
  #   data_measurements: dataframe with the measurements with at least those
  #                     columns (parameter, value, station, date)
  # return: same dataframs as data_measurements

  # Get the data from the reference stations
  data_measurements_pm25 <- data_measurements %>%
    dplyr::filter(substr(station, 1,2) == "NL" & parameter == "pm25")

  data_measurements_pm10 <- data_measurements %>%
    dplyr::filter(substr(station, 1,2) == "NL" & parameter == "pm10")

  # Rename the parameter pm -> pm_kal
  data_measurements_pm25_kal <- data_measurements_pm25 %>%
    dplyr::mutate(parameter = "pm25_kal")
  data_measurements_pm10_kal <- data_measurements_pm10 %>%
    dplyr::mutate(parameter = "pm10_kal")

  # Add the pm_kal to the data_measurements
  data_measurements <- dplyr::bind_rows(data_measurements,
                                        data_measurements_pm10_kal,
                                        data_measurements_pm25_kal)

  return(data_measurements)
}

add_uncertainty_sensor <- function(data_measurements, uc_pm10 = 8.5, uc_pm25 = 5.3){
  # Function to add a value of uncertainty to the sensor data,
  # default values from research Boeren en Buren.
  # arguments:
  #   data_measurements: dataframe
  #   uc_pm10: double, to indicate the uncertainty in pm10, default = 8.5
  #   uc_pm25: double, to indicate the uncertainty in pm25, default = 5.3
  # return: dataframe as data_measurements with extra column "sd",
  # this column is 0 for reference stations

  # Add uncertainty to the measurements of the sensors
  data_measurements <- data_measurements %>%
    dplyr::mutate(sd = dplyr::case_when(parameter == "pm25_kal" & !grepl("NL", station) ~ uc_pm25,
                                        parameter == "pm25" & !grepl("NL", station) ~ uc_pm25,
                                        parameter == "pm10_kal" & !grepl("NL", station) ~ uc_pm10,
                                        parameter == "pm10" & !grepl("NL", station) ~ uc_pm10,
                                        T ~ 0))
  return(data_measurements)
}

add_uncertainty_sensor_percent <- function(data_measurements, uc_pm10 = 20, uc_pm25 = 20, uc_min_pm10 = 8.5, uc_min_pm25 = 5.3){
  # Function to add a percentage of the value as uncertainty to the sensor data, with
  # a minimal uncertainty
  # arguments:
  #   data_measurements: dataframe
  #   uc_pm10: double, to indicate the percentage uncertainty in pm10, default = 20
  #   uc_pm25: double, to indicate the percentage uncertainty in pm25, default = 20
  #   uc_pm10: double, to indicate the minimal uncertainty in pm10, default = 8.5
  #   uc_pm25: double, to indicate the minimal uncertainty in pm25, default = 5.3
  # return: dataframe as data_measurements with extra column "sd",
  # this column is 0 for reference stations

  # Add uncertainty to the measurements of the sensors
  data_measurements <- data_measurements %>%
    dplyr::mutate(sd = dplyr::case_when(parameter == "pm25_kal" & !grepl("NL", station) ~ uc_pm25/100*value,
                                        parameter == "pm25" & !grepl("NL", station) ~ uc_pm25/100*value,
                                        parameter == "pm10_kal" & !grepl("NL", station) ~ uc_pm10/100*value,
                                        parameter == "pm10" & !grepl("NL", station) ~ uc_pm10/100*value,
                                        T ~ 0))
  # Check for the minimal uncertainty
  data_measurements <- data_measurements %>%
    dplyr::mutate(sd = dplyr::case_when(parameter == "pm25_kal" & !grepl("NL", station) & sd < uc_min_pm25 ~ uc_min_pm25,
                                        parameter == "pm25" & !grepl("NL", station) & sd < uc_min_pm25 ~ uc_min_pm25,
                                        parameter == "pm10_kal" & !grepl("NL", station) & sd < uc_min_pm10 ~ uc_min_pm10,
                                        parameter == "pm10" & !grepl("NL", station) & sd < uc_min_pm10 ~ uc_min_pm10,
                                        T ~ 0))
  return(data_measurements)
}
