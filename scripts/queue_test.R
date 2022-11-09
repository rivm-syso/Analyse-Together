######################################################################
# script to test download queue functions
######################################################################
# This script must be run interactive
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

# set working directory to root of repo
setwd(here::here())

# source scripts

source("./funs/database_fun.R")
source("./funs/queue_fun.R")
source("./funs/download_fun.R")
source("./scripts/test_functions.R")

# Connect with the database using pool, store data, read table              ====
    
fname_db <- get_database_path()
pool <- dbPool(

               drv = SQLite(),
               dbname = fname_db

)


# now let's start the queue
que <- task_q$new()

# and push some jobs to the queue
dbtables <- get_db_tables(pool)
for (i in 1:10) {
    rnd_station <- get_rnd_station(dbtables)
    qid <- que$push(dl_station, list(rnd_station$station,
                                     rnd_station$time_start,
                                     rnd_station$time_end), 
                    id = rnd_station$station)
    log_trace("pushed job {qid} to the queue")

}

# start queue
que$poll()

# see what's on the queue
tlist <- que$list_tasks()
print(tlist)

p <- station_overview(pool)
print(p)

t1 <- simple_task_list() %>%
    as.data.frame()
print(t1)




