######################################################################
#
#
#
# functions for the download queue
#
#
#
######################################################################

create_data_request <- function(kits, time_start, time_end, conn, max_requests = 100) {
    # Function to create a data request. A data request is a json doc
    # with a data.frame with 3 columns: kitid, time_start and
    # time_end. Each data request has an unique id and a index number.
    # Data requests can request max_requests kits/timestamp
    # combinations, if their are more kits/timestamps needed the
    # request is devided in sub requests. This function stores the
    # requests tot the database.
    #
    # kits: a vector of kits
    # time_start / time_end: a single value of time ranges, or a
    #   vector with the same length as kits with differen time ranges.
    # conn: database connection object
    # max_request: max jobs on queue for each request
    #
    # Return value: a list with data requests and last item is the job_id

    add_req <- function(x,y) {
        dl_req <- data.frame()
        for(i in 1:nrow(x))  {
            dl_req <- bind_rows(dl_req, data.frame(station = x$station[i],
                                                   time_start = x$time_start[i],
                                                   time_end = x$time_end[i], row.names = NULL))

        }
        return(dl_req)
    }


    # job_id <- sprintf("id%010.0f", round(runif(1, 1, 2^32), digits = 0))
    kits_req <- tibble(station = kits, time_start = time_start, time_end = time_end) %>%
        rowid_to_column("id") %>%
        mutate(set = ceiling(id / max_requests)) %>%
        group_by(set)

    res <- kits_req  %>% group_map(add_req)

    job_id <- sprintf("id%010.0f", round(runif(1, 1, 2^32), digits = 0))
    for(i in 1:length(res)) {
        job_id_seq <- sprintf("%s_%04i", job_id, i)

        if(!doc_exists(type = "data_req", ref = job_id_seq, conn = pool)) {
            log_trace("create_data_request: data request {job_id_seq} stored")
            add_doc(type = "data_req", ref = job_id_seq,
                    doc = res[[i]], conn = pool,
                    overwrite = TRUE)
        }
    }

    # Add job_id to return
    res <- c(res, job_id)
    invisible(res)

}





######################################################################
# R6 queue object, used by queue_manager
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
                                             log_trace("task_q: running duplicates {duplicates} tasks: {private$tasks$id[duplicates]}")
#                                              print(private$tasks$id[duplicates])
                                             for(i in duplicates) {
                                                 log_trace("task_q: create_data_reques: running {i}")
                                                 id <- gsub(".* ", "", private$tasks$id[i])
                                                 log_trace("task_q: new id {id}")
                                                 if (id %in% private$tasks$id) {
                                                     log_trace("task_q: new id {id} is duplicate")
                                                 } else {
                                                     log_trace("task_q: new id {id} is set to waiting")
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
                                                     log_trace("task_q: rename done task {i}, {private$tasks$id[i]}")
                                                     newid <- paste(private$tasks$seq[i],private$tasks$id[i])
                                                     private$tasks$id[i] <- newid
                                                 }
                                             }
                                         }


                                     }
                      )
)

