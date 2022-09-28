# misc functions to download and store data in the database

station_exists <- function(station, conn) {
    # checks if 'station' allready exists in the location table

    qry <- glue::glue_sql("select {station} from location;", .con = conn)
    res <- dbGetQuery(conn, qry)

    if(nrow(res)>=1) {
        result <- TRUE
    } else {
        result <- FALSE
    }

    return(result)

}



api_get_project_info <- function(project, conn) {
    # gets project info from the API
    # arguments:
    #   project: name of the project
    #   conn: db connection object

    log_debug(paste0("getting project info for ", project))
    projectinfo <- samanapir::GetSamenMetenAPIinfoProject(project)
    add_doc("project", project, projectinfo,
            conn = conn, overwrite = TRUE)
    return(projectinfo)
}

api_get_municipality_info <- function(municipality, conn) {
    # gets municipality info from the API
    # arguments:
    #   municipality: name of the municipality

    m <- get_doc("application", "municipalities", conn = conn) %>%
                  rename(code = X1, name = X2)

    gemid <- m %>%
          filter(name == gemeente) %>%
          pull(code) %>%
          as.character()

    log_debug(paste0("getting municipality info for ", municipality))
    muni_info <- samanapir::GetSamenMetenAPIinfoMuni(gemid)
    add_doc("municipality", municipality, muni_info,
            conn = conn, overwrite = TRUE)
    return(muni_info)
}


download_sensor_meta <- function(name, type, conn = pool) {
    # this function downloads a set of sensors belonging to either a
    # project or municipality,  download the station meta data, and
    # downloads the measurements for the requested time range
    # The type arguments determine if data is requested for a
    # municpality or a project.
    # arguments:
    #    name: name of project or municipality
    #    type: either 'project' or 'municipality'
    #    conn: db connection object

    #############
    # vectorized helper functions 

    insert_location_info_vectorized <- function(x, conn = conn) {
        kit <- x %>%
            as_tibble_row()  %>%
            mutate(lat = as.numeric(lat)) %>%
            mutate(lon = as.numeric(lon))
        log_trace("storing location info for {kit$kit_id}")
        insert_location_info(station = kit$kit_id,
                             lat = kit$lat,
                             lon = kit$lon,
                             conn)
    }

    store_sensor <- function(x, type = "station", conn = conn) {

        ref <- x[["kit_id"]]
        doc <- x %>%
            as_tibble_row()
        if(!doc_exists(type, ref, conn = conn)) {
            log_trace("storing meta data info for {ref}")
            add_doc(type, ref, doc, conn = conn)
        } else {
            log_trace("skipping store meta data info for {ref}")
        }

    }

    #############


    switch(type, 
           project = {
               projinfo <- api_get_project_info(project, conn = conn)
           }, 
           municipality = {
               projinfo <- api_get_municipality_info(project, conn = conn)
           },
           { #unknown type
               stop("download_sensor_meta: unknown type")
           })



    stations <- projinfo$sensor_data %>%
        select(kit_id, lat, lon) %>%
        as_tibble

    sensors_meta <- projinfo$sensor_data %>%
        select(-lat, -lon) %>%
        as_tibble()

    log_debug(paste("Got", nrow(stations), "stations for project", project))
    apply(stations, 1, FUN  = insert_location_info_vectorized, conn = conn)


    v1 <- apply(sensors_meta, 1, FUN=store_sensor, conn=conn)


    datastreams <- projinfo$datastream_data

    for(i in unique(datastreams$kit_id)) {
        kit <- datastreams %>%
            filter(kit_id == i) %>%
            select(-kit_id)


        type <- "datastream"
        if(!doc_exists(type, ref = i, conn = conn)) {
            log_trace("storing stream data info for {i}")
            add_doc(type, ref = i, doc = kit, conn = conn)
        } else {
            log_trace("skipping store stream data info for {i}")

        }

    }


}


get_stations_from_selection <- function(name, type, conn = pool) {
    # this function gets all the stations belonging to the selected
    # project of municipality. 
    # It returns a vector with stations ids (kit_ids), this vector can
    # then be used to download measurement data
    # The type arguments determine if data is requested for a
    # municpality or a project.
    # arguments:
    #    name: name of project or municipality
    #    type: either 'project' or 'municipality'
    #    conn: db connection object


    switch(type, 
           project = {
               info <- get_doc(type = "project", ref = name, conn = conn)
               kits <- info$sensor_data %>%
                   pull(kit_id)
           }, 
           municipality = {
               info <- get_doc(type = "municipality", ref = name, conn = conn)
               kits <- info$sensor_data %>%
                   pull(kit_id)
           },
           { #unknown type
               stop("download_sensor_meta: unknown type")
           })


    return(kits)
}

# 
# get_stations_from_project <- function(project) {
#     # this function gets all the stations belonging to a project. It
#     # returns a vector with stations ids (kit_ids), this vector can
#     # then be used to download measurement data
# 
#     info <- get_doc(type = "project", ref = project, conn = pool)
#     kits <- info$sensor_data %>%
#         pull(kit_id)
# 
#     return(kits)
# }
# 

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


