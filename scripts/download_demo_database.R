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


download_kits_data <- function(kits, time_start, time_end, pool) {
    # This function download sensor, meteo and air quality reference station data.
    # Based on the sensor meta data, meteo and reference data stations
    # are selected. For these stations the data is also downloaded. At
    # the end of the function, the locations of the meteo and ref.
    # station are stored in the db, if they don't allready exists
    # 
    # Please note, this function expects that meta data of the sensor
    # is allready present in the data base
    #
    # arguments:
    # kits: vector of kit id's
    # time_start, time_end: datetime objects with start and end date
    # pool: db connection object

    # some variables to use as counter and  store stationcodes
    counter <- 1
    knmicodes <- c()
    lmlcodes <- c()

    # for each kit ...
    for(i in kits) {

        # Download data
        log_debug("downloading measurements for station {i}, {counter}/{length(kits)}")
        date_range <- round_to_days(time_start, time_end)
        d <- download_data(i, Tstart = time_start, Tend = time_end,
                           fun = "download_data_samenmeten",
                           conn = pool)
        log_trace("got {nrow(d)} measurements")
        counter <- counter + 1

        # get meta data for kit, to get meteo and AQ station id's
        kitmeta <- get_doc(type = "station", ref = i, conn = pool)

        # Download meteo station data
        log_debug("downloading measurements for meteo station")
        date_range <- round_to_days(time_start, time_end)
        knmistation <- kitmeta %>% 
            select(knmicode)  %>%
            mutate(knmi_id = str_replace(knmicode, "knmi_06", "KNMI_")) %>%
            pull(knmi_id)
        knmicodes <- c(knmicodes, knmistation)
        d <- download_data(knmistation, Tstart = time_start, Tend = time_end,
                           fun = "download_data_knmi",
                           conn = pool)
        log_trace("got {nrow(d)} of KNMI measurements")
        log_trace("checking location for KNMI station  {i}")
        if(!station_exists(knmistation, conn = pool)) {
            log_trace("download and store location for KNMI station  {i}")
            loc <- download_locations_knmi(knmistationi, time_start, time_end)
            insert_location_info(station = loc[1,1],
                                 lat = loc[1,2],
                                 lon = loc[1,3],
                                 conn = pool)
        } 

        # download AQ reference station data
        log_debug("downloading measurements for AQ station")
        lmlstation <- kitmeta %>% 
            pull(pm10closecode)
        lmlcodes <- c(lmlcodes, lmlstation)
        d <- download_data(lmlstation, Tstart = time_start, Tend = time_end,
                           fun = "download_data_lml",
                           conn = pool)

        log_trace("checking location for LML station  {i}")
        if(!station_exists(lmlstation, conn = pool)) {
            log_trace("download an store  location for LML station  {i}")
            loc <- download_locations_lml(lmlstation)
            insert_location_info(station = loc[1, 1],
                                 lat = loc[1, 2],
                                 lon = loc[1, 3],
                                 conn = pool)
        }

    }


}

# Connect with the database using pool, store data, read table              ====
pool <- dbPool(

               drv = SQLite(),
               dbname = datafile("database.db")

)

# Download Amersfoort example data using SamenMeten API                    ====
project <- "Amersfoort"

time_start <- as_datetime("2022-01-01 00:00:00")
time_end <- as_datetime("2022-01-09 23:59:59")


#  get project info
download_project(project)

# get which kits are part of the project
kits <- get_stations_from_project(project)

download_kits_data(kits, time_start, time_end, pool)



