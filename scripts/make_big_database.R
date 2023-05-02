######################################################################
# This script creates a big (?) database filled with measurements for
# testing purposes
#
# !!!! This scripts destroys the current database !!!!
#
######################################################################



# Read in the necessary libraries                                           ====

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

# set working directory to root of repo

######################################################################
# config
######################################################################

# set time range
time_start <- as_datetime("2022-08-01 00:00:00")
time_end <- as_datetime("2022-10-31 23:59:59")

# time_start <- as_datetime("2023-01-01 00:00:00")
# time_end <- as_datetime("2023-03-31 23:59:59")
# 
# set municipalities to download
download_municipalities <- c("Nijmegen", "Eindhoven",
                             "Veenendaal", "Rotterdam",
                             "Zaanstad", "Amsterdam",
                             "Almere", "Amersfoort",
                             "Alkmaar")
                                
######################################################################
# start
######################################################################

# source scripts

source(here::here("funs","database_fun.R"))
source(here::here("funs","queue_fun.R"))
source(here::here("funs","download_fun.R"))
source(here::here("scripts","test_functions.R"))

# create new fresh database
source(here::here("scripts","init_database.R"))


# Connect with the database using pool, store data, read table              ====
    
fname_db <- get_database_path()
pool <- dbPool(

               drv = SQLite(),
               dbname = fname_db

)

# # Get all kits
for (i in download_municipalities) {
    download_sensor_meta(i, type = "municipality")
    kits <- get_stations_from_selection(i, type = "municipality")

    create_data_request(kits = kits,
                        time_start = time_start,
                        time_end = time_end,
                        conn = pool,
                        max_requests = 100)
}


