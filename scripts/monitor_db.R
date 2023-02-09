######################################################################
# Database monitoring script
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


# Connections with the database tables
measurements_con <- tbl(pool, "measurements")
stations_con <- tbl(pool, "location")
cache_con <- tbl(pool, "cache")

repeat{

    n_m <- nrow(measurements_con%>%collect())
    n_s <- nrow(stations_con%>%collect())
    n_c <- nrow(cache_con%>%collect())
    print(now())
    cat("Measurements:\t", n_m, "\n")
    cat("Stations:\t", n_s, "\n")
    cat("Cache:\t\t", n_c, "\n")
    cat("\n\n")
    Sys.sleep(5)


}
