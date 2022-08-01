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
  
  log_debug("downloading data for station {station} for time range {ts_api} -  {te_api}")
  
  knmi_all <- samanapir::GetKNMIAPI(station, ts_api, te_api)
  
  knmi_measurements <- knmi_all$data %>% as.data.frame() %>% select(-c('YYYYMMDD', 'H')) %>% 
    rename("station" = "STNS", "wd" = "DD", "ws" = "FF", "temp" = "TEMP", "rh" = "U", "timestamp" = "tijd")
  
  knmi_measurements$station <- paste0("KNMI_", knmi_measurements$station)
  
  knmi_measurements <- knmi_measurements %>% pivot_longer(cols = c("wd", "ws", "temp", "rh"), names_to = "parameter", values_to = "value") %>% 
    drop_na() %>% mutate(aggregation = 3600)
  
  return(knmi_measurements)
  
}


download_locations_knmi <- function(knmi_stations, time_start, time_end) {
  
  knmi_stations_all <- samanapir::GetKNMIAPI(knmi_stations, format(time_start, '%Y%m%d'), format(time_end, '%Y%m%d'))
  
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
  
  lml_data <- lml_data %>% rename("station" = "station_number", "timestamp" = "timestamp_measured", "parameter" = "formula") %>%
    drop_na() %>% mutate(aggregation = 3600) %>% mutate(parameter = tolower(parameter))
  
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

# Debug
if(interactive()) {


    install_github <- TRUE # set to FALSE if you run into github API limits

    # Read in the necessary libraries                                           ====

    # Tidyverse (essential)
    library(tidyverse)

    # Databases (essential)
    library(RSQLite)
    library(pool)

    library(lubridate)

    # logger
    library(logger)
    log_threshold(TRACE)

    # set data location
    library(datafile)
    datafileInit()

    library(samanapir)
    library(ATdatabase)

    # We need knmi_stations, this shouldn't be here but loaded from
    # file or database.
knmi_stations <- c(269, 209, 215, 225, 235, 240, 242, 248, 249, 251, 257, 258, 260, 267, 270, 273, 275, 277, 278, 279, 280, 283, 285, 286, 290, 308, 310, 312, 313, 315, 316, 319, 324, 330, 340, 343, 344, 348, 350, 356, 370, 375, 377, 380, 391)

    # Set language and date options                                             ====

    options(encoding = "UTF-8")                  # Standard UTF-8 encoding
    Sys.setlocale("LC_TIME", 'dutch')            # Dutch date format
    Sys.setlocale('LC_CTYPE', 'en_US.UTF-8')     # Dutch CTYPE format


    # Connect with the database using pool, store data, read table              ====
    pool <- dbPool(

                   drv = SQLite(),
                   dbname = datafile("database.db")

    )

    # Download Amersfoort example data using SamenMeten API                    ====
    project <- "Amersfoort"

    time_start <- as_datetime("2022-01-01 00:00:00")
    time_end <- as_datetime("2022-01-06 23:59:59")

    download_project(project)

    kits <- get_stations_from_project(project)

    counter <- 1
    for(i in kits) {
        log_debug("downloading measurements for station {i}, {counter}/{length(kits)}")
        date_range <- round_to_days(time_start, time_end)
        d <- download_data(i, Tstart = time_start, Tend = time_end,
                           fun = "download_data_samenmeten",
                           conn = pool)
        log_trace("got {nrow(d)} measurements")
        counter <- counter + 1
    }


  
  # Download meteo example data using KNMI API                               ====
    # use same time start/end as above
  
  kits <- knmi_stations
  
  counter <- 1
  for(i in kits) {
    log_debug("downloading measurements for station {i}, {counter}/{length(kits)}")
    date_range <- round_to_days(time_start, time_end)

    d <- download_data(i, Tstart = time_start, Tend = time_end,
                       fun = "download_data_knmi",
                       conn = pool)

    log_trace("got {nrow(d)} measurements")
    counter <- counter + 1
  }

  knmi_stations_locations <- download_locations_knmi(kits, time_start, time_end)
  
  for (i in 1:nrow(knmi_stations_locations))
  {

    insert_location_info(station = knmi_stations_locations[i,1],
                         lat = knmi_stations_locations[i,2],
                         lon = knmi_stations_locations[i,3],
                         conn = pool)

  }
  
  # test_lml_stations <- c("NL01494", "NL10437", "NL01491", "NL01494", "NL10444", "NL01491")
  meta <- tbl(pool, "meta") %>% as.data.frame()
  lml_stations <- get_lmlstations_from_meta(meta)
  
  counter <- 1
  for(i in lml_stations) {
    log_debug("downloading measurements for station {i}, {counter}/{length(lml_stations)}")
    date_range <- round_to_days(time_start, time_end)
    
    d <- download_data(i, Tstart = time_start, Tend = time_end,
                       fun = "download_data_lml",
                       conn = pool)
    
    log_trace("got {nrow(d)} measurements")
    counter <- counter + 1
  }
  
  
  df <- data.frame()
  for (i in lml_stations){

    lml_stations <- download_locations_lml(i)
    lml_stations_locations <- rbind(df, lml_stations)
    
  }
  
  for (i in 1:nrow(lml_stations_locations))
  {
    
    insert_location_info(station = lml_stations_locations[i,1],
                         lat = lml_stations_locations[i,2],
                         lon = lml_stations_locations[i,3],
                         conn = pool)
    
  }
  

}

