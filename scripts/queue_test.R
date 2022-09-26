######################################################################
# script to test download queue functions
######################################################################
# This script must be run interactive
######################################################################

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

# set working directory to root of repo
setwd(here())

# source scripts

source("./funs/database_fun.R")
source("./funs/queue_fun.R")
source("./scripts/test_functions.R")

# Connect with the database using pool, store data, read table              ====
pool <- dbPool(

               drv = SQLite(),
               dbname = datafile("database.db")

)


# dl_station function should be replaced with function developed
# in #42
dl_station <- function(id, time_start, time_end) {

    library(tidyverse)
    library(lubridate)
    library(RSQLite)
    library(pool)
    library(logger)
    library(glue)
    library(datafile)
    datafileInit()
    library(samanapir)
    library(ATdatabase)

    #load functions
    source("./funs/database_fun.R")

    fname_db <- datafile("database.db") 
    conn <- DBI::dbConnect(drv = SQLite(), dbname = fname_db)

    sqliteSetBusyHandler(conn, 10000) # time out in ms in case db is locked

    cat("Running dl_station, id:", id, ", start:", as_datetime(time_start),
        ", end:", as_datetime(time_end), "\n")

    # Download data
    log_debug("downloading measurements for station {i}")
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

    log_debug("downloading measurements for AQ station")
    lmlstation <- kitmeta %>%
        pull(pm10closecode)
    d <- download_data(lmlstation, Tstart = time_start, Tend = time_end,
                       fun = "download_data_lml",
                       conn = conn)

    log_trace("checking location for LML station  {lmlstation}")
    if(!station_exists(lmlstation, conn = conn)) {
        log_trace("download an store  location for LML station  {lmlstation}")
        loc <- download_locations_lml(lmlstation)
        insert_location_info(station = loc[1, 1],
                             lat = loc[1, 2],
                             lon = loc[1, 3],
                             conn = conn)
    }

    cat("download completed, disconnecting db\n")
    DBI::dbDisconnect(conn)

}

# now let's start the queue
que <- task_q$new()

# and push some jobs to the queue
dbtables <- get_db_tables(pool)
for (i in 1:10) {
    rnd_station <- get_rnd_station(dbtables)
    qid <- que$push(dl_station, list(rnd_station$station,
                                     rnd_station$time_start,
                                     rnd_station$time_end), 
                    id = rnd_station$station)
    log_trace("pushed job {qid} to the queue")

}

# start queue
que$poll()

# see what's on the queue
tlist <- que$list_tasks()
print(tlist)

p <- station_overview(pool)
print(p)


