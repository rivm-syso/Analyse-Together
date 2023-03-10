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
#sqliteSetBusyHandler(pool, 10e3)

# get some testing data to create jobs, add jobs to meta database

dbtables <- get_db_tables(pool)


for(i in 1:4) {
    job_id <- sprintf("id%010.0f", round(runif(1, 1, 2^32), digits = 0))
    print(job_id)

    dl_req <- data.frame()
    for (i in 1:10) {
        rnd_station <- get_rnd_station(dbtables)
        dl_req <- bind_rows(dl_req, data.frame(station = rnd_station$station,
                                               time_start = rnd_station$time_start,
                                               time_end = rnd_station$time_end, row.names = NULL))
    }

    add_doc(type = "data_req", ref = job_id, doc = dl_req, conn = pool, overwrite = TRUE)
    x <- get_doc(type = "data_req", ref = job_id, conn = pool)
}


poolClose(pool)
