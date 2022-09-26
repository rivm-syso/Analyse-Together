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
source(here("funs","download_fun.R"))

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
time_end <- as_datetime("2022-01-03 23:59:59")


#  get project info
download_project(project)

# get which kits are part of the project
kits <- get_stations_from_project(project)

for(i in kits) {
    dl_station(i, time_start, time_end)
}



