######################################################################
# Example download function
######################################################################

dl_station <- function(id, time_start, time_end) {

    # load libraries (do I really need tidyverse?)
    library(tidyverse)
    library(lubridate)
    library(RSQLite)
    library(pool)
    library(logger)
    library(glue)
    library(datafile)
    datafileInit()
    library(samanapir)
    library(ATdatabase)

    #load functions
    source("../Analyse-Together/funs/database_fun.R")

    # connect to db
    fname_db <- datafile("database.db") 
    conn <- DBI::dbConnect(drv = SQLite(), dbname = fname_db)
    sqliteSetBusyHandler(conn, 10000) # time out in ms in case db is locked

    # start data downloads
    d <- download_data(id, Tstart = time_start, Tend = time_end,
                       fun = "download_data_samenmeten",
                       conn = conn)

    # disconnect / cleanup
    DBI::dbDisconnect(conn)

}


