# Functions to get the start datasets (default by loading the application)

#' get measurement data cleaned
#'
#' get the measurement data from the caching dataset and do some cleaning steps,
#' like removing NA values
#'
#' @param measurements_con tbl, to indicate the database table
#' @param stations_name character, with the stations names of interest
#' @param parameter_input chacracter whit whic parameter of interest
#' @param start_time date, to indicatidate start of the period
#' @param end_time date, to indicate end of the period
#'
#' @return dataframe data.frame(station = character(0),
#' parameter = character(0), value = numeric(0), aggregation = numeric(0),
#' timestamp = integer(0), date = structure(numeric(0), tzone = "Europe/Amsterdam",
#'                  class = c("POSIXct", "POSIXt")))
#' @export
#'
get_measurements_cleaned <- function(measurements_con,
                                     stations_name,
                                     parameter_input,
                                     start_time,
                                     end_time){
  # get the measurements from the caching dbs
  data_all <- get_measurements(measurements_con,
                               stations_name,
                               start_time,
                               end_time)
  # Remove duplicates
  data_all <- data_all %>%
    # drop the ID column
    dplyr::select(-c(id)) %>%
    dplyr::distinct()

  # Remove NA values
  data_all <-data_all[!is.na(data_all$value), ]

  # Create a pm10_kal and pm25_kl for reference stations
  data_all <- add_ref_kal(data_all)

  # Add uncertainty to the measurements of the sensors
  data_all <- add_uncertainty_sensor_percent(data_all)

  # Add bias to the uncertainty sensors raw data
  data_all <- add_uncertainty_bias_sensor(data_all)

  # Select only the measurements of the given parameter
  data_all <- filter_parameter(data_all, parameter_input)

  return(data_all)

}

#' get stations cleaned
#' Get all the station information from the given station_names
#' Set for the stations which have data (are in measurements_data)
#' the colour and line and groupname and type
#'
#' @param stations_con tbl, connection to database table
#' @param stations_name character, the station names of interest
#' @param measurements_data df, with at least column named station
#' @param col_default character, with default colour for stations
#' @param line_default character, with linetype
#' @param group_name_none character, with name for default group
#' @param line_overload character, with linetype
#'
#' @return named list with 2 df named "data" and "data_all"
#' @export
#'
get_stations_cleaned <- function(stations_con,
                                 stations_name,
                                 measurements_data,
                                 col_default,
                                 line_default,
                                 group_name_none,
                                 line_overload){
  # Get the information from the stations
  data_stations_data_all <- get_locations(stations_con, stations_name)

  # Select only the station containing data
  stations_with_data <- measurements_data %>%
    dplyr::select(station) %>%
    unique() %>% pull()

  # Take for each sensor 1 location and add the plot-colours etc.
  data_stations_data <- data_stations_data_all %>%
    dplyr::filter(station %in% stations_with_data) %>%
    dplyr::distinct(station, .keep_all = T) %>%
    # Add some specific info for the tool
    dplyr::mutate(selected = F, col = col_default, linetype = line_default,
                  station_type = "sensor", group_name = group_name_none,
                  label = station, stroke = col) %>%
    dplyr::mutate(station_type = ifelse(grepl("KNMI", station) == T, "KNMI",
                                        ifelse(grepl("^NL.[0-9].", station) == T, "ref",
                                               station_type))) %>%
    dplyr::mutate(linetype = ifelse(station_type == "ref", line_overload, linetype),
                  size = ifelse(station_type == "ref", 2,1))

  return(list(data = data_stations_data,
              data_all = data_stations_data_all))

}
