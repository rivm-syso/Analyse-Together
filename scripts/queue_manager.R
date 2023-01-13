######################################################################
# script to test download queue manager 
######################################################################
# This script runs the queue manager in a idle while loop. The queue
# manager takes a data request from the database and starts the
# downloadprocesses to get data from the API. To put data request in
# the datbase, see script create_data_request.R
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

source(here("funs","database_fun.R"))
source(here("funs","queue_fun.R"))
source(here("funs","download_fun.R"))
source(here("scripts","test_functions.R"))

# Connect with the database using pool, store data, read table              ====
    
fname_db <- get_database_path()
pool <- dbPool(

               drv = SQLite(),
               dbname = fname_db

)

list_doc <- function(type, conn) {

    qry <- glue::glue_sql("SELECT ref FROM meta WHERE type={type};", 
                          .con = conn)
    res <- dbGetQuery(conn, qry)
    return(res$ref)

}


    que <- task_q$new()
    # get single job

    while(TRUE) {

        joblist <- list_doc(type = "data_req", conn = pool)
        if(length(joblist) == 0) {
            log_trace("queue_man: no data requests")
            Sys.sleep(5)
            next
        }
        job_id <- joblist[1]
        j <- get_doc(type = "data_req", ref = job_id, conn = pool)

        # create queue, run jobs, wait until finished, collect stats


        for (i in 1:nrow(j)) {

            qid <- que$push(dl_station, list(j$station[i],
                                             j$time_start[i],
                                             j$time_end[i]), 
                            id = j$station[i])
            log_trace("pushed job {qid} to the queue")
            que$poll()
        }

        time_spent <- system.time(

                                  while(nrow(que$list_tasks()) >4) {
                                      while(!is.null(que$pop())) {
                                          que$pop()
                                      }
                                      Sys.sleep(3)
                                  }
        )

        j_done <- list(j, data.frame(sec = c(time_spent)))
        remove_doc(type = "data_req", ref = job_id, conn = pool)
        add_doc(type = "data_req_done", ref = job_id, doc = j_done, conn = pool)
    }

