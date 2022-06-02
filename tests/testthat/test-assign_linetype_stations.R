# Tests for functions
test_that("assign_linetye_stations", {
  source('../../funs/assign_linetype_stations.R')
  # linetypes for the stations
  line_cat <- list('dashed', 'dotted', 'dotdash')#, 'longdash', 'twodash')
  line_default <- 'blank'
  line_overload <- 'solid'
  line_station_type <- "ref"

  # Selected stations
  stations_selected <- c("SSK_LH022","SSK_LD017","SSK_LD016","SSK_LD015", "SSK_LD014")

  # Create an input data set
  input_data <- structure(list(id = 1:10,
                               station = c("SSK_LH022", "SSK_LD017",  "SSK_LD016", "SSK_LD015", "SSK_LD014", "SSK_LD013", "SSK_LD012",  "SSK_LD011", "SSK_LD010", "SSK_LD009"),
                               station_type = c("ref", "ref",  "ref", "ref", "ref", "ref", "ref",  "ref", "ref", "ref"),
                               lat = c(51.476, 51.894,  51.867, 51.924, 51.915, 51.937, 51.93, 51.916, 51.977, 51.959),
                               lon = c(3.959, 4.488, 4.355, 4.484, 4.46, 4.48, 4.436, 4.498, 4.138, 4.503),
                               timestamp = c(1650965547.69878, 1650965547.72012, 1650965547.72253, 1650965547.72474, 1650965547.72708, 1650965547.72938, 1650965547.73159, 1650965547.73387, 1650965547.73642, 1650965547.7388),
                               selected = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                               linetype = c("#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000")),
                          row.names = c(NA, 10L),
                          class = "data.frame")

  # Select multiple stations ====
  # Set some stations slected = T
  input_data <- input_data %>% dplyr::mutate(selected = (dplyr::case_when(station %in% stations_selected ~ T,
                                                                          T ~ F)))

  # Get output form the function
  output_data <- assign_linetype_stations(input_data, line_cat, line_default, line_overload, line_station_type)

  # Test the unselected stations
  expect_equal(output_data %>% dplyr::filter(!(station  %in% stations_selected)) %>% dplyr::select(linetype) %>% dplyr::pull(), c("blank", "blank", "blank", "blank", "blank"))

  # Test the selected -> use first linetype, and last station doesn't have a linetype, so use overload linetype
  expect_equal(output_data %>% dplyr::filter(station %in% stations_selected) %>% dplyr::select(linetype) %>% dplyr::pull(), c('dashed', 'dotted', 'dotdash','solid', 'solid'))

  # Deselect station ====
  stations_selected <- c("SSK_LD017","SSK_LD016","SSK_LD015", "SSK_LD014")
  # Set some stations slected = T
  input_data <- output_data %>% dplyr::mutate(selected = (dplyr::case_when(station %in% stations_selected ~ T,
                                                                           T ~ F)))
  # Get output form the function
  output_data <- assign_linetype_stations(input_data, line_cat, line_default, line_overload, line_station_type)

  # Test the unselected stations
  expect_equal(output_data %>% dplyr::filter(!(station %in% stations_selected)) %>% dplyr::select(linetype)%>% dplyr::pull(), c("blank", "blank", "blank", "blank", "blank", "blank"))

  # Test the selected: the linetype stay the same for each station The linetype form the overload is replaced when available
  expect_equal(output_data %>% dplyr::filter(station %in% stations_selected) %>% dplyr::select(linetype) %>% dplyr::pull(), c('dotted', 'dotdash','dashed','solid'))

})
