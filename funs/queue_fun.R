######################################################################
# functions fro the download queue
######################################################################



task_q <- R6::R6Class(
                      "task_q",
                      public = list(
                                    initialize = function(concurrency = 4L) {
                                        private$start_workers(concurrency)
                                        invisible(self)
                                    },
                                    list_tasks = function() private$tasks,
                                    get_num_waiting = function()
                                        sum(!private$tasks$idle & private$tasks$state == "waiting"),
                                    get_num_running = function()
                                        sum(!private$tasks$idle & private$tasks$state == "running"),
                                    get_num_done = function() sum(private$tasks$state == "done"),
                                    is_idle = function() sum(!private$tasks$idle) == 0,

                                    push = function(fun, args = list(), id = NULL) {
                                        # if (is.null(id)) id <- private$get_next_id()
                                        nextid <- private$get_next_id()
                                        if (is.null(id)) id <- nextid
                                        #if (id %in% private$tasks$id) stop("Duplicate task id")
                                        if (id %in% private$tasks$id) {
                                            idstate = "duplicate"
                                            id <- paste(nextid, id, sep = " ")
                                        } else {
                                            idstate = "waiting"
                                        }
                                        before <- which(private$tasks$idle)[1]
                                        private$tasks <- tibble::add_row(private$tasks, .before =
                                                                         before,
                                                                     id = id, seq = nextid, idle = FALSE, state = idstate, fun = list(fun),
                                                                     args = list(args), worker = list(NULL), result = list(NULL))
                                        private$schedule()
                                        invisible(id)
                                    },

                                    poll = function(timeout = 0) {
                                        limit <- Sys.time() + timeout
                                        as_ms <- function(x)
                                            if (x==Inf) -1 else as.integer(as.double(x, "secs") * 1000)
                                        repeat{
                                            topoll <- which(private$tasks$state == "running")
                                            conns <- lapply(
                                                            private$tasks$worker[topoll],
                                                            function(x) x$get_poll_connection())
                                            pr <- processx::poll(conns, as_ms(timeout))
                                            private$tasks$state[topoll][pr == "ready"] <- "ready"
                                            private$schedule()
                                            ret <- private$tasks$id[private$tasks$state == "done"]
                                            if (is.finite(timeout)) timeout <- limit - Sys.time()
                                            if (length(ret) || timeout < 0) break;
                                        }
                                        ret
                                    },

                                    pop = function(timeout = 0) {
                                        if (is.na(done <- self$poll(timeout)[1])) return(NULL)
                                        row <- match(done, private$tasks$id)
                                        result <- private$tasks$result[[row]]
                                        private$tasks <- private$tasks[-row, ]
                                        c(result, list(task_id = done))
                                    }
                                    ),

                      private = list(
                                     tasks = NULL,
                                     next_id = 1L,
                                     get_next_id = function() {
                                         id <- private$next_id
                                         private$next_id <- id + 1L
                                         paste0(".", id)
                                     },

                                     start_workers = function(concurrency) {
                                         private$tasks <- tibble::tibble(
                                                                         id = character(), seq = character(), idle = logical(),
                                                                         state = c("waiting", "running", "ready", "done")[NULL],
                                                                         fun = list(), args = list(), worker = list(), result = list())
                                         for (i in seq_len(concurrency)) {
                                             rs <- callr::r_session$new(wait = FALSE)
                                             private$tasks <- tibble::add_row(private$tasks,
                                                                              id = paste0(".idle-", i), seq = ".0", idle = TRUE, state = "running",
                                                                              fun = list(NULL), args = list(NULL), worker = list(rs),
                                                                              result = list(NULL))
                                         }
                                     },

                                     schedule = function() {
                                         ready <- which(private$tasks$state == "ready")
                                         if (!length(ready)) return()
                                         rss <- private$tasks$worker[ready]

                                         private$tasks$result[ready] <- lapply(rss, function(x) x$read())
                                         private$tasks$worker[ready] <- replicate(length(ready), NULL)
                                         private$tasks$state[ready] <-
                                             ifelse(private$tasks$idle[ready], "waiting", "done")

                                         waiting <- which(private$tasks$state == "waiting")[1:length(ready)]
                                         private$tasks$worker[waiting] <- rss
                                         private$tasks$state[waiting] <- ifelse(private$tasks$idle[waiting], "ready", "running")
                                         lapply(waiting, function(i) {
                                                    if (! private$tasks$idle[i]) {
                                                        private$tasks$worker[[i]]$call(private$tasks$fun[[i]],
                                                                                       private$tasks$args[[i]])
                                                    }
                                                          
                                        })

                                         duplicates <- which(private$tasks$state == "duplicate")
                                         if(length(duplicates) >=1) {
                                             log_trace("running duplicates {duplicates} tasks: {private$tasks$id[duplicates]}")
                                             print(private$tasks$id[duplicates])
                                             for(i in duplicates) {
                                                 log_trace("running {i}")
                                                 id <- gsub(".* ", "", private$tasks$id[i])
                                                 log_trace("new id {id}")
                                                 if (id %in% private$tasks$id) {
                                                     log_trace("new id {id} is duplicate")
                                                 } else {
                                                     log_trace("new id {id} is set to waiting")
                                                     private$tasks$id[i] <- id
                                                     private$tasks$state[i] <- "waiting"
                                                 }
                                                 
                                             }
                                         }

                                         donetasks <- which(private$tasks$state == "done")
                                         if(length(donetasks) >=1 ) {
                                             log_trace("running donetasks {donetasks} tasks: {private$tasks$id[donetasks]}")
                                             for(i in donetasks) {
                                                 oldid <- private$tasks$id[i]
                                                 if(!grepl("^\\.", oldid)) {
                                                     log_trace("rename done task {i}, {private$tasks$id[i]}")
                                                     newid <- paste(private$tasks$seq[i],private$tasks$id[i])
                                                     private$tasks$id[i] <- newid
                                                 }
                                             }
                                         }


                                     }
                      )
)




######################################################################
# prototyping code
######################################################################



get_db_tables <- function(conn) {

    dbtbls <- list(measurements = tbl(conn,"measurements") %>% collect() ,
                   sensor = tbl(conn, "location") %>% collect(),
                   meta = tbl(conn, "meta") %>% collect(),
                   cache = tbl(conn, "cache") %>% collect()
    )

    return(dbtbls)

}



if(interactive()) {
    source("./funs/database_fun.R")

    # dl_station function should be replaced with function developed
    # in #42
    dl_station <- function(id, time_start, time_end) {

        library(tidyverse)
        library(lubridate)
        library(RSQLite)
        library(pool)
        library(logger)
        library(glue)
        library(datafile)
        datafileInit()
        library(samanapir)
        devtools::load_all("../ATdatabase")

        #load functions
        source("./funs/database_fun.R")

        fname_db <- datafile("database.db") 
        conn <- DBI::dbConnect(drv = SQLite(), dbname = fname_db)

        sqliteSetBusyHandler(conn, 10000) # time out in ms in case db is locked
        cat("Running dl_station, id:", id, ", start:", as_datetime(time_start),
            ", end:", as_datetime(time_end), "\n")


        d <- download_data(id, Tstart = time_start, Tend = time_end,
                           fun = "download_data_samenmeten",
                           conn = conn)
        cat("download completed, disconnecting db\n")


        DBI::dbDisconnect(conn)

    }

    # this function creates a randowm set of a station and time range,
    # this can be used to test the download function
    get_rnd_station <- function(dbtables, pool) {

        stations <- dbtables$measurements %>% 
            filter(!str_detect(station, "KNMI")) %>%
            distinct(station) %>%
            pull
        s <- sample(stations, size = 1)

        last_day <- dbtables$measurements %>%
            filter(station == s) %>%
            select(timestamp) %>%
            max() %>%
            as_datetime()
        last_day

        first_day <- dbtables$measurements %>%
            filter(station == s) %>%
            select(timestamp) %>%
            min() %>%
            as_datetime()
        first_day


        time_start <- last_day
        time_end <- last_day + days(round(runif(1, min = 1, max = 5)))
        time_range <- round_to_days(time_start, time_end)
        time_range
        res <- list(station = s,
                    last_day = last_day,
                    time_start = time_range[1],
                    time_end = time_range[2])
        return(res)
    }

    dbtables <- get_db_tables(pool)
    rnd_station <- get_rnd_station(dbtables, pool)
    rnd_station


    # now let's start the queue
    que <- task_q$new()

    # and push some jobs to the queue
    for (i in 1:10) {
        rnd_station <- get_rnd_station(dbtables, pool)
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


}


# create a plot of existing time ranges
if(FALSE) {
    
    dbtables <- get_db_tables(pool)
    cache <- dbtables$cache %>%
        mutate(tstart = as_datetime(start)) %>%
        mutate(tend = as_datetime(end)) %>%
        select(station, tstart, tend)

    p <- ggplot(cache) +
      geom_segment(aes(x = tstart, xend = tend,
                         y = station, yend = station ), 
                     color = "gray") +
      geom_point(aes(x = tstart, y = station), 
                 color = "green", size = 4) +
                      geom_point(aes(x = tend, y = station), 
                                 color = "red", size = 2) 
    print(p)


}


