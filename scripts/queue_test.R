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

# Connect with the database using pool, store data, read table              ====
pool <- dbPool(

               drv = SQLite(),
               dbname = datafile("database.db")

)


get_db_tables <- function(conn) {

    dbtbls <- list(measurements = tbl(conn,"measurements") %>% collect() ,
                   sensor = tbl(conn, "location") %>% collect(),
                   meta = tbl(conn, "meta") %>% collect(),
                   cache = tbl(conn, "cache") %>% collect()
    )

    return(dbtbls)

}

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


    d <- download_data(id, Tstart = time_start, Tend = time_end,
                       fun = "download_data_samenmeten",
                       conn = conn)
    cat("download completed, disconnecting db\n")


    DBI::dbDisconnect(conn)

}

# this function creates a randowm set of a station and time range,
# this can be used to test the download function
get_rnd_station <- function(dbtables, pool) {

    stations <- dbtables$measurements %>% 
        filter(!str_detect(station, "KNMI")) %>%
        distinct(station) %>%
        pull
    s <- sample(stations, size = 1)

    last_day <- dbtables$measurements %>%
        filter(station == s) %>%
        select(timestamp) %>%
        max() %>%
        as_datetime()
    last_day

    first_day <- dbtables$measurements %>%
        filter(station == s) %>%
        select(timestamp) %>%
        min() %>%
        as_datetime()
    first_day


    time_start <- last_day
    time_end <- last_day + days(round(runif(1, min = 1, max = 5)))
    time_range <- round_to_days(time_start, time_end)
    time_range
    res <- list(station = s,
                last_day = last_day,
                time_start = time_range[1],
                time_end = time_range[2])
    return(res)
}

dbtables <- get_db_tables(pool)
rnd_station <- get_rnd_station(dbtables, pool)
rnd_station


# now let's start the queue
que <- task_q$new()

# and push some jobs to the queue
for (i in 1:10) {
    rnd_station <- get_rnd_station(dbtables, pool)
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




# create a plot of existing time ranges

dbtables <- get_db_tables(pool)
cache <- dbtables$cache %>%
    mutate(tstart = as_datetime(start)) %>%
    mutate(tend = as_datetime(end)) %>%
    select(station, tstart, tend)

p <- ggplot(cache) +
    geom_segment(aes(x = tstart, xend = tend,
                     y = station, yend = station ), 
                 color = "gray") +
geom_point(aes(x = tstart, y = station), 
           color = "green", size = 4) +
geom_point(aes(x = tend, y = station), 
           color = "red", size = 2) 
print(p)


