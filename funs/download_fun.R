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
    library(samanapir)
    library(ATdatabase)

    #load functions
    source("./funs/database_fun.R")

    fname_db <- get_database_path()
    conn <- DBI::dbConnect(drv = SQLite(), dbname = fname_db)

    sqliteSetBusyHandler(conn, 10000) # time out in ms in case db is locked

    cat("Running dl_station, id:", id, ", start:", as_datetime(time_start),
        ", end:", as_datetime(time_end), "\n")

    # Download data
    log_debug("downloading measurements for station {id}")
    date_range <- round_to_days(time_start, time_end)
    d <- download_data(id, Tstart = time_start, Tend = time_end,
                       fun = "download_data_samenmeten",
                       conn = conn)
    log_trace("got {nrow(d)} measurements")

    # get meta data for kit, to get meteo and AQ station id's
    kitmeta <- get_doc(type = "station", ref = id, conn = conn)

    # Download meteo station data
    log_debug("downloading measurements for meteo station")
    date_range <- round_to_days(time_start, time_end)
    knmistation <- kitmeta %>%
        select(knmicode)  %>%
        mutate(knmi_id = str_replace(knmicode, "knmi_06", "KNMI_")) %>%
        pull(knmi_id)
    d <- download_data(knmistation, Tstart = time_start, Tend = time_end,
                       fun = "download_data_knmi",
                       conn = conn)
    log_trace("got {nrow(d)} of KNMI measurements")
    log_trace("checking location for KNMI station")
    if(!station_exists(knmistation, conn = conn)) {
        log_trace("download and store location for KNMI station  {knmistation}")
        loc <- download_locations_knmi(knmistation, time_start, time_end)
        insert_location_info(station = loc[1, 1],
                             lat = loc[1, 2],
                             lon = loc[1, 3],
                             conn = conn)
    }

    lmlstation <- kitmeta %>% select(pm10closecode, pm25closecode) %>%
        as.data.frame() %>% as.character()

    log_trace("Downloading measurements for AQ station. pm10 station: {lmlstation[1]}, pm25 station: {lmlstation[2]}")
    for(j in unique(lmlstation)) {
        log_trace("downloading for LML station  {j}")
        print(lmlstation)
        print(unique(lmlstation))
        d <- download_data(j, Tstart = time_start, Tend = time_end,
                           fun = "download_data_lml",
                           conn = conn)

        log_trace("checking location for LML station  {j}")
        if(!station_exists(j, conn = conn)) {
            log_trace("download an store  location for LML station  {j}")
            loc <- download_locations_lml(j)
            insert_location_info(station = loc[1, 1],
                                 lat = loc[1, 2],
                                 lon = loc[1, 3],
                                 conn = conn)
        }
    }

    cat("download completed, disconnecting db\n")
    DBI::dbDisconnect(conn)

}


