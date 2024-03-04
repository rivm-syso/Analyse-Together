######################################################################
# This script is run during the docker build
#
# If needed it creates a new database with a minimal dataset. It also
# creates new data requests to get additional data when the
# application starts.
#
######################################################################


# Read in the necessary libraries                                 ====

# Tidyverse (essential)
library(tidyverse)

# Databases (essential)
library(RSQLite)
library(pool)
library(lubridate)
library(here)
setwd(here::here())

# logger
library(logger)
log_threshold(TRACE)

remotes::install_github('rivm-syso/samanapir', ref = 'main')
library(samanapir)
library(ATdatabase)

# source scripts

source(here::here("funs","database_fun.R"))
source(here::here("funs","queue_fun.R"))
source(here::here("funs","download_fun.R"))
source(here::here("scripts","test_functions.R"))

######################################################################
# configuration
######################################################################
# Almere: 01-09-2023 tot 01-12-2023
time_start_default <- lubridate::ymd("20230901")
time_end_default <- lubridate::ymd("20231201")
city <- "Almere"

######################################################################
# Chack database creation
######################################################################

# A new database must be created when:
#  1) no database exists
#  2) a 'renew_database' file exists

create_db <- FALSE

db.path <- get_database_path()

if(!file.exists(db.path)) {
    cat("DATAPREP: no database found\n")
    create_db <- TRUE
}

renew_database_file <- file.path(dirname(db.path), "renew_database")
if(file.exists(renew_database_file)) {
    cat("DATAPREP: renew database file found\n")
    create_db <- TRUE
}


if(create_db) {
    cat("DATAPREP: new database created\n")

    # create new fresh database
    source(here::here("scripts","init_database.R"))
    
    # Connect with the database using pool, store data, read table  
    fname_db <- get_database_path()
    pool <- dbPool( drv = SQLite(), dbname = fname_db)


    download_sensor_meta(city, type = "municipality")
    kits <- get_stations_from_selection(city, type = "municipality")

    kitsample <- sample(kits, 10)
    for(i in kitsample) {
        log_trace(glue("downloading sample data for  kit {i}"))
        dl_station(i, as_datetime(time_start_default), 
                   as_datetime(time_start_default + days(1)))
    }

    create_data_request(kits = kits,
                        time_start = time_start_default,
                        time_end = time_end_default,
                        conn = pool,
                        max_requests = 100)
    unlink(renew_database_file)
}


