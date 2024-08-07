######################################################################
#
#
# Functions used by the queue_manager scheduler
#
#
######################################################################




run_scheduled <- function(type = "municipality", ref = "daily") {
    # This functions runs the schedule. It gets the data from the meta
    # table and creates the data requests.
    #
    # Arguments:
    # type: which type of to download, either municipality or project
    # ref: reference to the meta doc containing the list to download,
    # currently only 'daily' is supeorted

    sched <- get_doc(type = "schedule", ref, con = pool)

    if (is.logical(sched) && is.na(sched)) {
        log_info(glue("WARNING: run_scheduled: no schedule found for type {type}"))
        return()
    }

    run_sched <- sched |>
        filter(type == {{type}} )|>
        select(name, days)

    for(i in seq_len(nrow(run_sched))) {
        sname <- run_sched$name[i]
        sdays <- run_sched$days[i]
        log_trace(glue("run_scheduled: running {type}: {sname} for last {sdays} days"))

        sched_time_end <- lubridate::today()
        sched_time_start <- sched_time_end - lubridate::days(sdays)

        download_sensor_meta(sname, type)
        kits <- get_stations_from_selection(sname, type)

        create_data_request(kits = kits,
                            time_start = sched_time_start,
                            time_end = sched_time_end,
                            conn = pool,
                            max_requests = 100)
    }


}

