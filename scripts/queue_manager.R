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


library(samanapir)
library(ATdatabase)
library(here)

# set working directory to root of repo
setwd(here::here())

# source scripts

source(here::here("funs","database_fun.R"))
source(here::here("funs","queue_fun.R"))
source(here::here("funs","download_fun.R"))
source(here::here("funs","logging_fun.R"))
source(here::here("scripts","test_functions.R"))

# setup logging
pid <- Sys.getpid()
logfile <- file.path(get_database_dirname(),paste0("queue.log"))
log_appender(appender_file(logfile))
log_info("Queue_manager started")
set_loglevel()


# Connect with the database using pool, store data, read table              ====

fname_db <- get_database_path()
pool <- dbPool(
               drv = SQLite(synchronous = "off"),
               dbname = fname_db
)
pool::dbExecute(pool, "PRAGMA busy_timeout = 90000")
pool::dbExecute(pool, "PRAGMA synchronous = 1 ")

while(TRUE) {

    joblist <- list_doc(type = "data_req", conn = pool)
    if(length(joblist) == 0) {
        log_trace("queue_man: no data requests")
        Sys.sleep(5)
        next
    }

    # start queue
    que <- task_q$new()

    # get job
    job_id <- joblist[1]
    j <- ATdatabase::get_doc(type = "data_req", ref = job_id, conn = pool)
    log_debug("queue_man: data request {job_id} found")
    log_trace("queue_man: total of {length(joblist)} of data requests still waiting")

    # create queue, run jobs, wait until finished, collect stats


    for (i in 1:nrow(j)) {

        qid <- que$push(dl_station, list(j$station[i],
                                         as_datetime(j$time_start[i]),
                                         as_datetime(j$time_end[i])), 
                        id = j$station[i])
        log_trace("queue_man: pushed job {qid} to the queue")
    }

    Sys.sleep(1)
                                  
    log_debug("queue_man: starting queue with {nrow(que$list_tasks())} jobs on queue")


    counter <- 0
    last_njobs <- 0
    watchdog_counter <- 0
    success <- FALSE


    time_spent <- system.time(

                              while(nrow(que$list_tasks()) >4) {
                                  njobs <- nrow(que$list_tasks()) - 4
                                  counter <- counter + 1
                                  if(counter%%10 == 0) {
                                      log_debug("queue_man: still {njobs} jobs on queue")
                                      if (last_njobs == njobs) {
                                          watchdog_counter  <- watchdog_counter + 1
                                      }
                                      if(watchdog_counter > 10) {
                                          log_warn("WARNING queue_man: queue stalled, watchdog activated")
                                          break
                                      }
                                      last_njobs  <- njobs
                                  }

                                  repeat{
                                      res <- que$pop()
                                      if(!is.null(res)) {
                                          if(!is.null(res$error)) {
                                              log_warn("ERROR queue_man:  dl_station: res$task_id\n {res$error}")
                                              # print(res)
                                          } else {
                                              log_trace("queue_man: task popped, result found for task {res$id}")
                                          }
                                      } else {
                                          log_trace("queueman: task pop returned NULL")
                                          success <- TRUE
                                          break
                                      }
                                  }
                                  Sys.sleep(5)
                              }
    )

    rm(que); gc()

    if(success) {
        log_debug("queue_man: queue finished with success")
        j_done <- list(j, data.frame(sec = c(time_spent)))
        log_debug("queue_man: data request {job_id} done")
        remove_doc(type = "data_req", ref = job_id, conn = pool)
        ATdatabase::add_doc(type = "data_req_done", ref = job_id, doc = j_done, conn = pool)
    } else {
        log_warn("WARNING: queue_man: queue finished without success, retrying")
    }
}


