#' Get the locations and coordinates from the stations and convert to
#'  spatialcoordinates
#'
#' @param data_stations_data dataframe with at least the columns c(station, lon, lat)
#'
#' @return list with "station_loc" and "station_loc_coord"; if no stations found,
#' the statiosn_loc and station_loc_coord contain value NULL
#' @export
get_locations_coordinates <- function(data_stations_data){
  # Check if there is data
  if(nrow(data_stations_data) == 0){
    station_loc = NULL
    station_loc_coord = NULL
  }else{

    # Get the location of the stations
    station_loc <- data_stations_data %>%
      dplyr::distinct(station, .keep_all = T) %>%
      dplyr::filter(lon > 0 & lat >0)

    # Convert to spatialploints
    station_loc_coord <- SpatialPointsDataFrame(station_loc[,c('lon','lat')],
                                                station_loc)
  }

  return(list(station_loc = station_loc, station_loc_coord = station_loc_coord))

}
