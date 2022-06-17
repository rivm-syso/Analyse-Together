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

download_data_samenmeten <- function(x, station, conn = pool) {

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
        Sys.sleep(1) # don't hammer the API
    }

    if(nrow(d)>0) {
        d <- d %>%
            rename(station = kit_id) %>%
            mutate(aggregation = 3600)
    } else {
        d <- NULL
    }
    log_debug("got {nrow(d)} measurements")
    return(d)
}



# Debug
if(interactive()) {

    project <- "HEI"

    time_start <- as_datetime("2022-01-01 00:00:00")
    time_end <- as_datetime("2022-01-03 23:59:59")

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
        count <- count + 1
    }

}


