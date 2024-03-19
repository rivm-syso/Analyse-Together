###############################################
### Metadata per parameter Module ###
###############################################

# Creates metadata tables based on selected input, for 1 parameter

###################################################################
### Output Module ####
#################################################################
metadata_param_output <- function(id) {

  ns <- NS(id)

  dataTableOutput(ns("meta_table"))

}


######################################################################
# Server Module
######################################################################
#
#

metadata_param_server <- function(id,
                            data_measurements,
                            data_stations,
                            parameter,
                            selected_start_date,
                            selected_end_date,
                            name_munproj) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Get all the info together and create dataframe
    metadata_table <- reactive({
      # Get the data measurements
      data_all <- data_measurements()

      # Check if there is data
      shiny::validate(
        need(!is_empty(data_all),'Geen sensordata beschikbaar.'),
        need(!dim(data_all)[1] == 0,'Geen sensordata beschikbaar.')
      )

      # Get the info of the stations
      data_stations_part <- data_stations() %>%
        dplyr::select(c(station, station_type, label, group_name, col, selected)) %>%
        dplyr::distinct(station, .keep_all = T)

      # Get the selected time period and the number of hours
      start_time <- selected_start_date()
      end_time <- selected_end_date()

      timerange <- difftime(end_time, start_time, unit = "hours")

      # Prepare the data for the table - do some summarise
      metadata_table <- data_all %>%
        # Take the data of the chosen parameter
        dplyr::filter(parameter == parameter() |
                        # To include the KNMI stations
                        parameter == "wd") %>%
        dplyr::group_by(station) %>%
        # Check te date of the first and last measurement
        dplyr::mutate(first_m = as.POSIXct(as.numeric(min(timestamp)),
                                           origin='1970-01-01') %>%
                        format(., "%d %b %Y"),
                      last_m = as.POSIXct(as.numeric(max(timestamp)),
                                          origin='1970-01-01') %>%
                        format(., "%d %b %Y")) %>%
        # Summarise the data
        dplyr::summarise(first_m = first_m,
                         last_m = last_m,
                         n_obs = sum(!is.na(value)),
                         max_obs = as.numeric(timerange),
                         per_obs = round(n_obs/max_obs*100) ) %>%
        dplyr::distinct(station, .keep_all = T) %>%
        dplyr::ungroup()

      # Add the station information to the measurements
      # Keep all stations from the station_data
      metadata_table <- data_stations_part %>%
        dplyr::left_join(metadata_table, by = "station") %>%
        dplyr::select(c(station, group_name, per_obs, max_obs, first_m, last_m,
                        station_type, selected, col))

      # Only show selected station
      metadata_table <- metadata_table %>% dplyr::filter(selected)

      # Reorder so the selected are on top of the table
      metadata_table <- metadata_table %>%
        dplyr::arrange(desc(selected), group_name, station)

      return(metadata_table)
    })

    # Get the name of chosen municipality/project
    project_or_municipality <- reactive({
      project_or_municipality <- name_munproj()
      return(project_or_municipality)
    })

    # Define colour-breaks for table
    breaks_col <- reactive({
      # Set the breaks and the colours
      brks <- seq(0,100,10)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
        {paste0("rgb(", ., ", 243,", .,", 0.4)")}

      return(list(brks = brks, clrs = clrs))
    })


    output$meta_table <-

      DT::renderDataTable({
        # Get the data for the table
        data_for_table <- metadata_table()

        # Check if there is data
        shiny::validate(need(!is.null(data_for_table), "No data available."))


        # Get the colours for the names
        col_group <- data_for_table %>% dplyr::select(group_name, col) %>% unique() %>%
          # Set the colours for not selected stations and the ref stations to white in stead of black
          dplyr::mutate(col = ifelse(col == col_default, "#FFFFFF", col))

        # Select data for in the table to show
        data_for_table <- metadata_table() %>%
          dplyr::select(-c(selected, col, max_obs))

        # Text to set some information above the table
        caption_text <- paste0(i18n$t("word_table")," ",
                               " ", parameter() ," ",i18n$t("word_within"),
                               " ",project_or_municipality(), "<br> for the period: ",
                               selected_start_date() ," to ",
                               selected_end_date(), " .")

        # create the table
        try(DT::datatable(data_for_table,
                      colnames = c("Station" = "station",
                                   "Group" = "group_name",
                                   "Datacapture %" = "per_obs",
                                   "First measurements" = "first_m",
                                   "Last measurements" = "last_m",
                                   "Type" = "station_type"),
                      caption = HTML(caption_text),
                      options = list(scrollX = TRUE, pageLength = 12,
                                     lengthChange = FALSE), class = c('row-border', 'hover'),
                      rownames = FALSE) %>%
              # formatStyle("Number of measurements PM2.5", background = "white") %>%
              formatStyle(columns = c("Group"),
                          valueColumns = c("Group"),
                          backgroundColor = styleEqual(levels = col_group$group_name,
                                                       values = col_group$col)) %>%
              formatStyle(columns = c("Datacapture %"),
                          backgroundColor = styleInterval(cuts = breaks_col()$brks,
                                                          values = breaks_col()$clrs)
              )
        )})
  })
}
