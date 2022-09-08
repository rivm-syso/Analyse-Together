# misc functions to download and store data in the database


api_get_project_info <- function(project) {
    # gets project info from the API
    # arguments:
    #   project: name of the project

    log_debug(paste0("getting project info for ", project))
    projectinfo <- samanapir::GetSamenMetenAPIinfoProject(project)
    add_doc("project", project, projectinfo, conn = pool, overwrite = TRUE)
    return(projectinfo)
}


download_project <- function(project, Tstart, Tend) {
    # this function downloads a project. It gets the station id's
    # belonging to the project, download the station meta data, and
    # downloads the measurements for the requested time range
    # arguments:
    #    project: name of project
    #    Tstart: string with start date
    #    Tend: string with end data

    #############
    # vectorized helper functions 

    insert_location_info_vectorized <- function(x, conn) {
        kit <- x %>%
            as_tibble_row()  %>%
            mutate(lat = as.numeric(lat)) %>%
            mutate(lon = as.numeric(lon))
#TODO: add check to test if location exists
        log_trace("storing location info for {kit$kit_id}")
        insert_location_info(station = kit$kit_id,
                            lat = kit$lat,
                            lon = kit$lon,
                            conn)
    }

    store_sensor <- function(x, type = "station", conn = pool) {

        ref <- x[["kit_id"]]
        doc <- x %>%
            as_tibble_row()
        if(!doc_exists(type, ref, conn = pool)) {
            log_trace("storing meta data info for {ref}")
            add_doc(type, ref, doc, conn = pool)
        } else {
            log_trace("skipping store meta data info for {ref}")
        }

    }

    #############


    projinfo <- api_get_project_info(project)


    stations <- projinfo$sensor_data %>%
        select(kit_id, lat, lon) %>%
        as_tibble

    sensors_meta <- projinfo$sensor_data %>%
        select(-lat, -lon) %>%
        as_tibble()

    log_debug(paste("Got", nrow(stations), "stations for project", project))
    apply(stations, 1, FUN  = insert_location_info_vectorized, conn = pool)


    v1 <- apply(sensors_meta, 1, FUN=store_sensor, conn=pool)


datastreams <- projinfo$datastream_data

for(i in unique(datastreams$kit_id)) {
    kit <- datastreams %>%
        filter(kit_id == i) %>%
        select(-kit_id)


    type <- "datastream"
    if(!doc_exists(type, ref = i, conn = pool)) {
        log_trace("storing stream data info for {i}")
        add_doc(type, ref = i, doc = kit, conn = pool)
    } else {
        log_trace("skipping store stream data info for {i}")

    }

}


}

get_stations_from_project <- function(project) {
    # this function gets all the stations belonging to a project. It
    # returns a vector with stations ids (kit_ids), this vector can
    # then be used to download measurement data

    info <- get_doc(type = "project", ref = project, conn = pool)
    kits <- info$sensor_data %>%
        pull(kit_id)

    return(kits)
}


round_to_days <- function(time_start, time_end) {
    # the samen meten API requires time ranges in full days. This
    # function rounds any time to the start of the day of time_start
    # until the end of the day of time_end
    #
    # arguments:
    #   time_start: string with start time
    #   time_end: string with end time

    ts <- floor_date(time_start, unit = "day")
    te <- ceiling_date(time_end, unit = "day")

    if(!te>ts) {
        te <- te + days(1)
    }

    if(te<=ts) {
        stop("te < ts")
    }
    res <- c(ts, te)
    names(res) <- c("time_start","time_end")

    return(res)
}

download_data_samenmeten <- function(x, station, conn ) {

    streams <- get_doc(type = "datastream", ref = station, conn) %>%
        pull(datastream_id)
    streams_desc <- get_doc(type = "datastream", ref = station, conn) %>%
        select(datastream_id, kit_id_ext)

    ts_api <- strftime(as_datetime(x[1]), format="%Y%m%d")
    te_api <- strftime(as_datetime(x[2]), format="%Y%m%d")
    log_debug("downloading data for station {station} for time range {ts_api} -  {te_api}")

    d <- data.frame()
    for(i in streams) {
        m <- streams_desc %>% filter(datastream_id == i) %>%
            pull(kit_id_ext)
        log_trace("getting stream {i} {m}")
        
        res <- try(obs <- GetSamenMetenAPIobs(as.character(i),
                                   station, ts_api, te_api))
        if(!class(res) == "try-error") {
            if(nrow(res) > 9 ) {
            d <- bind_rows(d, obs)
            }
        } else {
            log_debug("API Error in stream {station} - {i}")
        }
        #Sys.sleep(1) # don't hammer the API
    }

    if(nrow(d)>0) {
        d <- d %>%
            rename(station = kit_id) %>%
            mutate(aggregation = 3600)
    } else {
        d <- NULL
    }
    log_debug("got {ifelse(is.null(d),'no',nrow(d))} measurements")
    return(d)
}


download_data_knmi <- function(x, station, conn) {
  
  ts_api <- strftime(as_datetime(x[1]), format="%Y%m%d")
  te_api <- strftime(as_datetime(x[2]), format="%Y%m%d")
  
  station_nr <- gsub(".*_", "", station)
  
  log_debug("downloading data for station {station_nr} for time range {ts_api} -  {te_api}")
  
  knmi_all <- samanapir::GetKNMIAPI(station_nr, ts_api, te_api)
  
  knmi_measurements <- knmi_all$data %>% as.data.frame() %>% select(-c('YYYYMMDD', 'H')) %>% 
    rename("station" = "STNS", "wd" = "DD", "ws" = "FF", "temp" = "TEMP", "rh" = "U", "timestamp" = "tijd")
  
  knmi_measurements$station <- paste0("KNMI_", knmi_measurements$station) 
  
  knmi_measurements <- knmi_measurements %>% pivot_longer(cols = c("wd", "ws", "temp", "rh"), names_to = "parameter", values_to = "value") %>% 
    drop_na() %>% mutate(aggregation = 3600)
  
  return(knmi_measurements)
  
}


download_locations_knmi <- function(knmi_stations, time_start, time_end) {
  
  station_nr <- gsub(".*_", "", knmi_stations)
  
  knmi_stations_all <- samanapir::GetKNMIAPI(station_nr, format(time_start, '%Y%m%d'), format(time_end, '%Y%m%d'))
  
  knmi_stations_locations <- knmi_stations_all$info %>% 
      as.data.frame() %>% 
      select(c("STNS", "LAT", "LON")) %>% 
      rename("station" = "STNS", "lat" = "LAT", "lon" = "LON") %>% 
      drop_na() %>% 
      mutate(lat = as.numeric(lat)) %>%
      mutate(lon = as.numeric(lon)) %>% 
      mutate(station = paste0("KNMI_", station))
  
  return(knmi_stations_locations)
  
}

get_lmlstations_from_meta <- function(meta_data){
  
  meta_doc <- meta_data %>% filter(type == "station") %>% select(doc)
  
  station_list <- c()
  
  for (i in 1:nrow(meta_doc)){
    station_list <- append(station_list, str_split(meta_doc[i, ], ","))
  }
  
  station_list <- unlist(station_list)
  station_list <- station_list[str_detect(station_list, "NL")] 
  station_list <- gsub('[^[:alnum:] ]', '', station_list)
  station_list <- Filter(function(x) str_length(x) < 8, station_list)
  station_list <- unique(station_list)
  
  return(station_list)
  
}

download_data_lml <- function(x, station, conn) {
  
  ts_api <- strftime(as_datetime(x[1]), format="%Y%m%d")
  te_api <- strftime(as_datetime(x[2]), format="%Y%m%d")
  
  log_debug("downloading data for station {station} for time range {ts_api} -  {te_api}")
  
  lml_data <- samanapir::GetLMLstatdataAPI(station, ts_api, te_api)
  
  if (length(lml_data) == 0){
    # Return empty dataframe if station returns no data
    
    lml_data <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(lml_data) <- c("station", "value", "timestamp", "parameter", "aggregation")
  }
  
  else{
    
    lml_data <- lml_data %>% rename("station" = "station_number", "timestamp" = "timestamp_measured", "parameter" = "formula") %>%
      drop_na() %>% mutate(aggregation = 3600) %>% mutate(parameter = tolower(parameter))
  }
  
  return(lml_data)
}

download_locations_lml <- function(stations) {
  
  lml_locations <- samanapir::GetLMLstatinfoAPI(stations)
  
  lml_locations <- lml_locations %>%
    select(c("station_number", "lat", "lon")) %>%
    drop_na() %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon))
  
  return(lml_locations)
  
}


