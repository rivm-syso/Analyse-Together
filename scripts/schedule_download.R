######################################################################
# This scripts is a prototype for launching scheduled data req.
######################################################################
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

# source scripts & functions
source(here::here("funs","database_fun.R"))
source(here::here("funs","queue_fun.R"))
source(here::here("funs","download_fun.R"))
source(here::here("funs","scheduler_fun.R"))
source(here::here("funs","logging_fun.R"))
source(here::here("scripts","test_functions.R"))

# setup logging
pid <- Sys.getpid()
logfile <- file.path(get_database_dirname(),paste0("queue.log"))
log_appender(appender_file(logfile))
log_info("schedule started")
set_loglevel(level = TRACE)



# Connect to database
fname_db <- get_database_path()
pool <- dbPool(
               drv = SQLite(synchronous = "off"),
               dbname = fname_db
)
pool::dbExecute(pool, "PRAGMA busy_timeout = 90000")
pool::dbExecute(pool, "PRAGMA synchronous = 1 ")

# read prepped list with municipalities and projects to schedule
fname_scheduled <- get_schedule_fname()
scheduled <- read_csv(fname_scheduled)
scheduled

# add list to meta data
add_doc(type = "schedule", ref = "daily", 
        doc = scheduled, con = pool, overwrite = TRUE)

sched_time_cfg <-  "13:00"
sched_time <- lubridate::hm(sched_time_cfg) + today()
tz(sched_time) <- "Europe/Amsterdam"
sched_time

    
res <- check_schedule(sched_time, force = TRUE)

