#' Assign color
#'
#' This function assigns color to the sensor from a given color
#' range. The sensor will keep the same color untill deselected, deselected
#' sensor get a default-color. The selected a color from the color-list.
#'
#' @param dataset df with at least columns station, selected, col
#' @param col_cat list with hexaconal color names (string)
#' @param col_default string with hexagonal color name, this color will be \
#' assigned if selected is False
#'
#' @return dataset with the col column filled properly
#' @export
#'
assign_color_stations <- function(dataset, col_cat, col_default, col_overload ){
  # Check which color are unselected
  # Assign default color to all unselected
  dataset <- dataset %>%
    dplyr::mutate(col = dplyr::case_when(!selected ~ col_default,
                                         T~col))

  # Check which sensors are selected
  # Check if they already have a color different then the default
  stations_new <- dataset %>% dplyr::filter(selected & !(col %in% col_cat)) %>% dplyr::select(station) %>% unique() %>% dplyr::pull()

  # Assign unused color to selected
  for(stat in stations_new){
    # Check which colors already are used
    col_used <- dataset %>% dplyr::select(col) %>% unique() %>% dplyr::pull()
    col_avail <-col_cat[!(col_cat %in% col_used)]
    # Check if there is still a color available
    if(purrr::is_empty(col_avail)){
      col_new <- col_overload # use color overload if no color is available
    }else{
      col_new <- col_avail[[1]]
    }

    # Assign color to station
    dataset <- dataset %>% dplyr::mutate(col = dplyr::case_when(station == stat ~ col_new,
                                                                T ~ col))
  }

  # Return dataset
  return(dataset)
}
