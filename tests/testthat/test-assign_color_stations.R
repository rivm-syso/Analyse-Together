# Tests for functions
test_that("assign_color_stations", {
  source('../../funs/assign_color_stations.R')
  # Colours for the stations
  col_cat <- list('#ffb612','#42145f','#777c00')
  col_default <- '#000000'
  col_overload <- '#111111'
  col_station_type <- "sensor"

  # Selected stations
  stations_selected <- c("SSK_LH022","SSK_LD017","SSK_LD016","SSK_LD015", "SSK_LD014")

  # Create an input data set
  input_data <- structure(list(id = 1:10,
                               station = c("SSK_LH022", "SSK_LD017",  "SSK_LD016", "SSK_LD015", "SSK_LD014", "SSK_LD013", "SSK_LD012",  "SSK_LD011", "SSK_LD010", "SSK_LD009"),
                               station_type = c("sensor", "sensor",  "sensor", "sensor", "sensor", "sensor", "sensor",  "sensor", "sensor", "sensor"),
                               lat = c(51.476, 51.894,  51.867, 51.924, 51.915, 51.937, 51.93, 51.916, 51.977, 51.959),
                               lon = c(3.959, 4.488, 4.355, 4.484, 4.46, 4.48, 4.436, 4.498, 4.138, 4.503),
                               timestamp = c(1650965547.69878, 1650965547.72012, 1650965547.72253, 1650965547.72474, 1650965547.72708, 1650965547.72938, 1650965547.73159, 1650965547.73387, 1650965547.73642, 1650965547.7388),
                               selected = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                               col = c("#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000")),
                          row.names = c(NA, 10L),
                          class = "data.frame")

  # Select multiple stations ====
  # Set some stations slected = T
  input_data <- input_data %>% dplyr::mutate(selected = (dplyr::case_when(station %in% stations_selected ~ T,
                                                                          T ~ F)))

  # Get output form the function
  output_data <- assign_color_stations(input_data, col_cat, col_default, col_overload,col_station_type )

  # Test the unselected stations
  expect_equal(output_data %>% dplyr::filter(!(station  %in% stations_selected)) %>% dplyr::select(col) %>% dplyr::pull(), c("#000000", "#000000", "#000000", "#000000", "#000000"))

  # Test the selected -> use first colors, and last station doesn't have a color, so use overload color
  expect_equal(output_data %>% dplyr::filter(station %in% stations_selected) %>% dplyr::select(col) %>% dplyr::pull(), c('#ffb612','#42145f','#777c00','#111111', '#111111'))

  # Deselect station ====
  stations_selected <- c("SSK_LD017","SSK_LD016","SSK_LD015", "SSK_LD014")
  # Set some stations slected = T
  input_data <- output_data %>% dplyr::mutate(selected = (dplyr::case_when(station %in% stations_selected ~ T,
                                                                          T ~ F)))
  # Get output form the function
  output_data <- assign_color_stations(input_data, col_cat, col_default, col_overload,col_station_type)

  # Test the unselected stations
  expect_equal(output_data %>% dplyr::filter(!(station %in% stations_selected)) %>% dplyr::select(col)%>% dplyr::pull(), c("#000000","#000000", "#000000", "#000000", "#000000", "#000000"))

  # Test the selected: the colours stay the same for each station The color form the overload is replaced when available
  expect_equal(output_data %>% dplyr::filter(station %in% stations_selected) %>% dplyr::select(col) %>% dplyr::pull(), c('#42145f','#777c00','#ffb612','#111111'))

})
