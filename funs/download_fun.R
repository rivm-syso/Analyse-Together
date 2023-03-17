######################################################################
# Example download function
######################################################################
dl_station <- function(id, time_start, time_end) {

    library(tidyverse)
    library(lubridate)
    library(RSQLite)
    library(pool)
    library(logger)
    library(glue)
    library(here)
    library(samanapir)
    library(ATdatabase)

    #load functions
    source(here("funs","database_fun.R"))

    pid <- Sys.getpid()
    logfile <- file.path(get_database_dirname(),paste0("download.log"))
    log_threshold(TRACE)
    log_appender(appender_file(logfile))

    fname_db <- get_database_path()
    conn <- DBI::dbConnect(drv = SQLite(synchronous = "off"), dbname = fname_db)

    sqliteSetBusyHandler(conn, 120000) # time out in ms in case db is locked

    log_debug("Running dl_station, id: {id} start: {as_datetime(time_start)}, end: {as_datetime(time_end)}")

    # Download data
    log_debug("downloading measurements for station {id}")
    date_range <- round_to_days(time_start, time_end)
    d <- ATdatabase::download_data(id, Tstart = time_start, Tend = time_end,
                       fun = "download_data_samenmeten",
                       conn = conn)
    log_trace("got {nrow(d)} measurements {id}")

    # get meta data for kit, to get meteo and AQ station id's
    kitmeta <- ATdatabase::get_doc(type = "station", ref = id, conn = conn)
    if(length(kitmeta) == 1 && is.na(kitmeta)) {
        log_warn("No meta data found for kit {id}", id)
        DBI::dbDisconnect(conn)
        return()
    } else {
        log_trace("got meta data for kit {kitmeta$kit_id}")
    }

    # Download meteo station data
    log_debug("downloading measurements for meteo station {id}")
    date_range <- round_to_days(time_start, time_end)
    knmistation <- kitmeta %>%
        select(knmicode)  %>%
        mutate(knmi_id = str_replace(knmicode, "knmi_06", "KNMI_")) %>%
        pull(knmi_id)
    d <- ATdatabase::download_data(knmistation, Tstart = time_start, Tend = time_end,
                       fun = "download_data_knmi",
                       conn = conn)
    log_trace("got {nrow(d)} of KNMI measurements {id}")
    log_trace("checking location for KNMI station {id}")
    if(!station_exists(knmistation, conn = conn)) {
        log_trace("download and store location for KNMI station  {knmistation} {id}")
        loc <- download_locations_knmi(knmistation, time_start, time_end)
        ATdatabase::insert_location_info(station = loc[1, 1],
                             lat = loc[1, 2],
                             lon = loc[1, 3],
                             conn = conn)
    }

    lmlstation <- kitmeta %>% select(pm10closecode, pm25closecode) %>%
        as.data.frame() %>% as.character()

    log_trace("Downloading measurements for AQ station. pm10 station: {lmlstation[1]}, pm25 station: {lmlstation[2]} {id}")
        
    for(j in unique(lmlstation)) {
        log_trace("downloading for LML station  {j} {id}")
        d <- ATdatabase::download_data(j, Tstart = time_start, Tend = time_end,
                           fun = "download_data_lml",
                           conn = conn)

    
        log_trace("got {nrow(d)} of LML measurements {id}")
        log_trace("checking location for LML station  {j} {id}")
        if(!station_exists(j, conn = conn)) {
            log_trace("download an store  location for LML station  {j} {id}")
            loc <- download_locations_lml(j)
            ATdatabase::insert_location_info(station = loc[1, 1],
                                 lat = loc[1, 2],
                                 lon = loc[1, 3],
                                 conn = conn)
        }
    }

    log_trace("download completed, disconnecting db {id}")
    DBI::dbDisconnect(conn)
    log_trace("dl_station returning {id}")
    cat("dl_station finished", id, "\n")
    return()

}


