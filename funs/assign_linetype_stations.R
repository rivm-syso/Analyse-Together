#' Assign linetype
#'
#' This function assigns linetype to the station from a given list linetypes
#' The station will keep the same linetype untill deselected. Used for station_type = ref.
#'
#' @param dataset df with at least columns station, selected, linetype
#' @param line_cat list with linetypes (strings)
#' @param line_default string with linetype, this linetype will be \
#' assigned if selected is False
#' @param line_overload if all options from the line_cat are in used, \
#' then this type is used.
#' @param linestation_type the type of the station you like to assign the linetype
#'
#' @return dataset with the linetype column filled properly
#' @export
#'
assign_linetype_stations <- function(dataset, line_cat, line_default, line_overload, line_station_type ){
  # Take only the station_type == ref
  dataset_ref <- dataset %>% dplyr::filter(station_type == line_station_type)

  # Check which stations are unselected
  # Assign default linetype to all unselected
  dataset_ref <- dataset_ref %>%
    dplyr::mutate(linetype = dplyr::case_when(!selected ~ line_default,
                                         T~linetype))

  # Check which stations are selected
  # Check if they already have a linetype different then the default
  stations_new <- dataset_ref %>%
    dplyr::filter(selected & !(linetype %in% line_cat)) %>%
    dplyr::select(station) %>%
    unique() %>%
    dplyr::pull()

  # Assign unused linetype to selected
  for(stat in stations_new){
    # Check which linetype already are used
    line_used <- dataset_ref %>%
      dplyr::select(linetype) %>%
      unique() %>%
      dplyr::pull()
    line_avail <- line_cat[!(line_cat %in% line_used)]
    # Check if there is still a linetype available
    if(purrr::is_empty(line_avail)){
      line_new <- line_overload # use linetype overload if no linetype is available
    }else{
      line_new <- line_avail[[1]]
    }

    # Assign linetype to station
    dataset_ref <- dataset_ref %>%
      dplyr::mutate(linetype = dplyr::case_when(station == stat ~ line_new,
                                                                T ~ linetype))
  }

  # Add the adjusted dataset_ref to the initial dataset
  dataset <- dataset %>% dplyr::filter(!(station_type == line_station_type)) %>% dplyr::bind_rows(., dataset_ref)

  # Return dataset
  return(dataset)
}
