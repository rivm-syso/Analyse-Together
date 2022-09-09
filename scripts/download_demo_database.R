######################################################################
# This scripts fills and existing database with some data for demo's
######################################################################
# use the init_databas script to create a new database
######################################################################



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

if(install_github) {
    remotes::install_github("rivm-syso/ATdatabase", ref = "develop",
                            build_opts ="")
}

library(ATdatabase)

# source functions
library(here)
setwd(here()) # set workdir to root
source(here("funs","database_fun.R"))
source(here("funs","queue_fun.R"))

# Set language and date options                                             ====

options(encoding = "UTF-8")                  # Standard UTF-8 encoding
Sys.setlocale("LC_TIME", 'dutch')            # Dutch date format
Sys.setlocale('LC_CTYPE', 'en_US.UTF-8')     # Dutch CTYPE format

# functions

station_exists <- function(station, conn) {

    qry <- glue::glue_sql("select {station} from location;", .con = conn)
    res <- dbGetQuery(conn, qry)

    if(nrow(res)>=1) {
        result <- TRUE
    } else {
        result <- FALSE
    }

    return(result)

}



# Connect with the database using pool, store data, read table              ====
pool <- dbPool(

               drv = SQLite(),
               dbname = datafile("database.db")

)

# Download Amersfoort example data using SamenMeten API                    ====
project <- "Amersfoort"

time_start <- as_datetime("2022-01-01 00:00:00")
time_end <- as_datetime("2022-01-06 23:59:59")


#  get project info
download_project(project)

# get which kits are part of the project
kits <- get_stations_from_project(project)



# downlod sensor data
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


# download KNMI and LML data, collect station ids so we can get the
# locations of the stations later on.
knmicodes <- c()
lmlcodes <- c()
for(i in kits) {
    log_debug("downloading measurements for station {i}, {counter}/{length(kits)}")
    date_range <- round_to_days(time_start, time_end)
    kitmeta <- get_doc(type = "station", ref = i, conn = pool)
    knmistation <- kitmeta %>% 
        select(knmicode)  %>%
        mutate(knmi_id = str_replace(knmicode, "knmi_06", "KNMI_")) %>%
        pull(knmi_id)

    knmicodes <- c(knmicodes, knmistation)

    d <- download_data(knmistation, Tstart = time_start, Tend = time_end,
                       fun = "download_data_knmi",
                       conn = pool)

    log_trace("got {nrow(d)} of KNMI measurements")

    lmlstation <- kitmeta %>% 
        pull(pm10closecode)

    lmlcodes <- c(lmlcodes, lmlstation)

    d <- download_data(lmlstation, Tstart = time_start, Tend = time_end,
                       fun = "download_data_lml",
                       conn = pool)

}


# store locations KNMI stations
knmi_stations_locations <- download_locations_knmi(unique(knmicodes), time_start, time_end)
for (i in unique(knmicodes)) {

    log_trace("checking location for KNMI station  {i}")

    if(!station_exists(i, conn = pool)) {
        log_trace("download and store location for KNMI station  {i}")
        loc <- download_locations_knmi(i, time_start, time_end)
        insert_location_info(station = loc[1,1],
                             lat = loc[1,2],
                             lon = loc[1,3],
                             conn = pool)
    } 

}


# store locations LML stations
for (i in unique(lmlcodes)) {

    log_trace("checking location for LML station  {i}")

    if(!station_exists(i, conn = pool)) {
    log_trace("download an store  location for LML station  {i}")
        loc <- download_locations_lml(i)

        insert_location_info(station = loc[1, 1],
                             lat = loc[1, 2],
                             lon = loc[1, 3],
                             conn = pool)
    }

}

