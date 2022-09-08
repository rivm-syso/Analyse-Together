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
library(ATdatabase)

# source functions
library(here)
setwd(here()) # set workdir to root
source(here("funs","database_fun.R"))
source(here("funs","queue_fun.R"))

# We need knmi_stations, this shouldn't be here but loaded from
# file or database.
knmi_stations <- c("KNMI_269", "KNMI_209", "KNMI_215", "KNMI_225", "KNMI_235", "KNMI_240", "KNMI_242", "KNMI_248", 
                   "KNMI_249", "KNMI_251", "KNMI_257", "KNMI_258", "KNMI_260", "KNMI_267", "KNMI_270", "KNMI_273", 
                   "KNMI_275", "KNMI_277", "KNMI_278", "KNMI_279", "KNMI_280", "KNMI_283", "KNMI_285", "KNMI_286", 
                   "KNMI_290", "KNMI_308", "KNMI_310", "KNMI_312", "KNMI_313", "KNMI_315", "KNMI_316", "KNMI_319", 
                   "KNMI_324", "KNMI_330", "KNMI_340", "KNMI_343", "KNMI_344", "KNMI_348", "KNMI_350", "KNMI_356", 
                   "KNMI_370", "KNMI_375", "KNMI_377", "KNMI_380", "KNMI_391")

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

# test_lml_stations <- c("NL01908", "NL01494", "NL10437", "NL01491", "NL01494", "NL10444", "NL01491")
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


lml_stations_locations <- data.frame()
for (i in lml_stations){

    lml_stations_lat_lon <- download_locations_lml(i)
    lml_stations_locations <- rbind(lml_stations_locations, lml_stations_lat_lon)

}

for (i in 1:nrow(lml_stations_locations))
{

    insert_location_info(station = lml_stations_locations[i,1],
                         lat = lml_stations_locations[i,2],
                         lon = lml_stations_locations[i,3],
                         conn = pool)

}


