######################################################################
# This scripts fills and existing database with some data for demo's
######################################################################
# use the init_databas script to create a new database
######################################################################



install_github_bool <- TRUE # set to FALSE if you run into github API limits

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

if(install_github_bool){
  devtools::install_github("https://github.com/rivm-syso/samanapir", ref='develop')
  devtools::install_github("https://github.com/rivm-syso/ATdatabase", ref="develop")
  devtools::install_github("https://github.com/jspijker/ATfun", ref="develop")
}
library(ATfun)
library(samanapir)
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
    
fname_db <- ATfun::get_database_path()
pool <- dbPool(

               drv = SQLite(),
               dbname = fname_db

)

# Download Amersfoort example data using SamenMeten API                    ====
project <- "Amersfoort"

time_start <- as_datetime("2026-10-08 00:00:00")
time_end <- as_datetime("2026-10-10 23:59:59")


#  get project info
ATfun::download_sensor_meta(project, type = "project", conn=pool)

# get which kits are part of the project
kits <- ATfun::get_stations_from_selection(project, type = "project")

for(i in kits) {
    dl_station(i, time_start, time_end)
}


if (FALSE) {
    # This takes quite a while to donwload, there are many sensors
    cat("Running extra code\n")
    gemeente <- "Amersfoort"
    ATfun::download_sensor_meta(gemeente, type = "municipality")
    kits <- ATfun::get_stations_from_selection(gemeente, type = "municipality")
    for(i in kits) {
        dl_station(i, time_start, time_end)
    }
}


