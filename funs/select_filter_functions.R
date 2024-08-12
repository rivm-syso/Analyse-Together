# Functions for selecting and filtering the data

#' Get selected stations ----
#'
#' Get the names of the stations which are selected
#'
#' @param data_stations df
#'
#' @return string with names of stations
#' @export
#'
get_selected_station <- function(data_stations){
  # Check if there is data
  shiny::validate(need(!is.null(data_stations), "No data_stations"))

  # Get the names of selected stations
  selected_station <- data_stations %>%
    dplyr::filter(selected == T) %>%
    dplyr::select(station) %>%
    pull()

  return(selected_station)
}


#' Filter data measurements
#'
#' Filter the data on selected stations, time and parameter
#'
#' @param start_time date, start of the selected period
#' @param end_time date, end of the selected period
#' @param cut_off numeric, values above this value will not be selected
#' @param parameter string, with the parameter of interest "pm25_kal"
#' @param data_stations df, with at least the columns c(date, stations,
#'  value, group_name, linetype, station_type, col, size, label)
#' @param data_all df, with at least the columns c(station, parameter,
#'  value, date, sd)
#'
#' @return df with the measurements and info of the stations selected,
#' columns : c(station, date, parameter, value, sd, label, group_name,
#' col, size, station_type, linetype)
#' @export
filter_data_measurements_fun <- function(start_time,
                                     end_time,
                                     cut_off,
                                     selected_parameter,
                                     data_stations,
                                     data_all){

  # Get selected stations
  selected_stations <- get_selected_station(data_stations)

  # Check if everything is available for the selection
  shiny::validate(need(!is.null(start_time) &
                         !is.null(end_time) &
                         !is.null(selected_parameter) &
                         !is.null(data_all) &
                         !purrr::is_empty(selected_stations),
                       "Not yet data selected" ) )

  # Get the info for each selected station
  station_info <- data_stations %>%
    dplyr::filter(selected == T)

  # Filter the measurements
  measurements_filt_stns <- data_all %>%
    dplyr::filter(date > start_time & date < end_time &
                    station %in% selected_stations &
                    parameter == selected_parameter &
                    value < cut_off)

  # Combine station_info with the measurements and keep relevant
  # columns
  measurements_filt_stns <-
    dplyr::left_join(measurements_filt_stns,
                     station_info, by = "station") %>%
    dplyr::select(station, date, parameter, value, sd, label,
                  group_name, col, size, station_type, linetype) %>%
    # Keep for this dataset the label the same as the station. No changes for grouping yet.
    dplyr::mutate(label = station)

  # log_trace("mod com: number of selected stations {length(selected_stations)}")
  # log_trace("mod com: names of selected stations {paste(selected_stations, sep = ' ', collapse = ' ')}")
  log_trace("mod com: filtered measurements {nrow(measurements_filt_stns)}")
  return(measurements_filt_stns)
}


#' Calculate group mean
#'
#' Calculate the mean values per time step for each group
#'  an minimum values is applied
#'
#' @param measurements df with the data measurements including  c(station, date, parameter, value, sd, label, group_name,
#' col, size, station_type, linetype)
#' @param uc_min_pm10 minimum value for a pm10 measurement
#' @param uc_min_pm25 minimum value for a pm2.5 measurement
#'
#' @return df with for each goup 1 measurement per time step
#' @export
calc_group_mean_fun <- function(measurements,
                            uc_min_pm10,
                            uc_min_pm25){

  # check if stations are selected
  shiny::validate(need(!is_empty(measurements) &
                         !dim(measurements)[1] == 0,
                       "No data_stations"))

  # Calculate group mean and sd
  data_mean <- measurements %>%
    # Set label to groupname
    dplyr::mutate(label = dplyr::case_when(station_type ==
                                             "sensor" ~ group_name,
                                           T ~ station)) %>%
    # Keep also the parameters for the plotting
    dplyr::group_by(group_name, date, parameter, label, col,
                    size, station_type, linetype) %>%
    dplyr::summarise(value = mean(value, na.rm = T),
                     number = n(),
                     sd = mean(sd, na.rm = T)/sqrt(n())) %>%
    dplyr::ungroup()

  # Set sd of a sensor to a minimal value, different for pm10 and pm25
  data_mean <- data_mean %>%
    # Check for minimal sd for sensors
    dplyr::mutate(
      sd = dplyr::case_when(station_type == "sensor" &
                              grepl("pm25", parameter, fixed = T) &
                              sd < uc_min_pm25 ~ uc_min_pm25,
                            station_type == "sensor" &
                              grepl("pm10", parameter, fixed = T) &
                              sd < uc_min_pm10 ~ uc_min_pm10,
                            T ~ sd))

  return(data_mean)

}


# Get knmi measurements ----
# the knmi measurements are excluded
# by the selected parameter in the measurements_filt_stns
#' Get selected knmi measurements
#'
#' Get the data of the selecetd knmi stations
#'
#' @param start_time date, start of the period
#' @param end_time date, end of the period
#' @param data_all df, with the measurements
#' @param data_stations df, with station info at least columns c(station, selected)
#'
#' @return df, part of data_all with the selected stations
#' @export
#'
#' @examples
get_knmi_measurements_fun <- function(start_time,
                                      end_time,
                                      data_all,
                                      data_stations
                                      ){

  # Get selected stations
  all_selected_stations <- get_selected_station(data_stations)
  selected_stations <- all_selected_stations[grep("KNMI", all_selected_stations)]

  # Check if everything is available for the selection
  shiny::validate(need(!is.null(start_time) &
                         !is.null(end_time) &
                         !is.null(data_all) &
                         !purrr::is_empty(selected_stations),
                       "Not yet data selected" ) )

  # Filter the measurements
  measurements_filt_knmi <- data_all %>%
    dplyr::filter(date > start_time & date < end_time &
                    station %in% selected_stations
    )

  # log_trace("mod com: number of selected stations {length(selected_stations)}")
  # log_trace("mod com: names of selected stations {paste(selected_stations, sep = ' ', collapse = ' ')}")
  log_trace("mod com: filtered measurements KNMI {nrow(measurements_filt_knmi)}")

  return(measurements_filt_knmi)
}
