#' Change the state of station to deselect
#'
#' Function to change the state of a station to deselect, including default group
#' name, colour and line type.
#'
#' @param data_stns : df with at least columns c(selected, group_name, station,
#' col, stroke, linetype, label, )
#' @param id_selected : string with the station name in it as in column "station"
#' in "data_stns"
#' @param group_name_none : string with the name of the group you have as default
#' for non-selected stations
#' @param col_default: string with the hexan colour code for non-selected stations
#' @param line_default: string with line type for non-selected stations
#'
#'
#' @return data_stns with mutated columns of the "id_selected"
#' @export
#'
change_state_to_deselected <- function(data_stns,
                                       id_selected,
                                       group_name_none,
                                       col_default,
                                       line_default){

  # Set the deselected station to select == F
  data_stns <- data_stns %>%
    dplyr::mutate(selected = ifelse(station == id_selected, F, selected),
                  # Remove group name and label
                  group_name = ifelse(station == id_selected, group_name_none, group_name),
                  label = ifelse(station == id_selected, station, label),
                  # Change the color to the default
                  col = ifelse(station == id_selected, col_default, col),
                  stroke = col,
                  # Change the linetype to the default
                  linetype = ifelse(station == id_selected, line_default, linetype))

  # Return the adjusted data_stns
    return(data_stns)
}


#' Change state to select
#'
#' To change the state of the station to selected in the data_stations df,
#' including change of group_name, label, colour and linetype
#'
#' @param data_stns : df with at least the columns c(station, selected,
#' group_name, col, stroke, label, startion_type)
#' @param id_selected : strign with the name of the station as kan be found in
#' data_stns$station
#' @param group_name : string name of the group in which the station will be assigned
#' @param col_select : string hexan colour code for the station colour
#' @param line_cat  : list with strings of line types to assign one of them
#' @param line_default :  string with default line type
#' @param line_overload : string with line type if all types of line_cat are used
#'
#' @return df identique to data_stns only the columns of id_selected are changed
#' @export
#'
change_state_to_selected <- function(data_stns,
                                     id_selected,
                                     group_name,
                                     col_select,
                                     line_cat,
                                     line_default,
                                     line_overload
                                     ){


  # Get the group name
  get_group_name <- group_name

  # Set the selected station to select == T
  data_stns <- data_stns %>%
    dplyr::mutate(selected = ifelse(station == id_selected, T, selected),
                  group_name = ifelse(station == id_selected & station_type == "sensor",
                                      get_group_name,
                                      ifelse(station == id_selected & station_type != "sensor",
                                             station,
                                             group_name)),
                  # Change the color to the col_select
                  col = ifelse(station == id_selected  & station_type == "sensor",
                               col_select, col),
                  stroke = col,
                  label = ifelse(station == id_selected & station_type == "sensor",
                                 get_group_name,
                                 ifelse(station == id_selected & station_type != "sensor",
                                        station,
                                        label)))

  # Assign linetype -> reference station
  data_stns <- assign_linetype_stations(data_stns, line_cat, line_default,
                                        line_overload, line_station_type = "ref")

  # Return the data_stns
  return(data_stns)
}
