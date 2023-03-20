######################################################################
# script to test download queue manager 
######################################################################
# This script creates 5 data request, based on a random selection of
# existing. Each data request contains 10 stations for which a small
# time range is requested
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

library(samanapir)
library(ATdatabase)
library(here)

# set working directory to root of repo
setwd(here::here())

# source scripts

source(here::here("funs","database_fun.R"))
source(here::here("funs","queue_fun.R"))
source(here::here("funs","download_fun.R"))
source(here::here("scripts","test_functions.R"))

# Connect with the database using pool, store data, read table              ====
    
fname_db <- get_database_path()
pool <- dbPool(

               drv = SQLite(),
               dbname = fname_db

)

# get some testing data to create jobs, add jobs to meta database

dbtables <- get_db_tables(pool)

kits <- data.frame()
for(i in 1:10) {
    rnd_station <- get_rnd_station(dbtables)
    kits <- rbind(kits, data.frame(station = rnd_station$station,
                                   time_start = rnd_station$time_start,
                                   time_end = rnd_station$time_end),
                                    row.names = NULL)
}

create_data_request(kits$station, time_start = kits$time_start, time_end = kits$time_end,
                        conn = pool, max_requests = 3)


poolClose(pool)
