######################################################################
#
#
# Functions used by the queue_manager scheduler
#
#
######################################################################


get_schedule_fname <- function() {
    # gets the filename of the schedule, either from an environment var
    # or use the default value 
    if ("ANALYSETOGETHER_SCHEDULE" %in% names(Sys.getenv())) {
        sched_fname <- Sys.getenv("ANALYSETOGETHER_SCHEDULE")
    } else {
        sched_fname <- file.path(here::here(), "prepped_data", "schedule.csv")
    }

    if(!file.exists(sched_fname)) {
        stop("ERROR: get_schedule_fname: path to filename not found")
    }
    return(sched_fname)
}



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
        log_warn(glue("WARNING: run_scheduled: no schedule found for type {type}"))
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


check_schedule <- function(scheduled_time, force = FALSE) {
    # checks if the current time is passed the scheduled time, if
    # that's true the function updates the last run meta doc and
    # returns TRUE. This only happens once a day
    # If the force option is used, the triggering of the function is
    # forced by setting the last_run data to yesterday. This is for
    # debug purposes
    # arguments:
    # scheduled time: the schedulted time after this function should
    # trigger once
    # force: set last run to yesterday
    # NOTE: take care of the timezone of scheduled time. It is
    # compared to the current time in the timezone of the running
    # system

    
    if(force) {
        last_schedule_run <- lubridate::today() - 1
        add_doc(type = "schedule", ref = "last_run", doc = last_schedule_run,
                overwrite = TRUE, conn = pool)
        return(FALSE)

    }

    t_now <- lubridate::now()
    t_last <- get_doc(type = "schedule", ref = "last_run", conn = pool)

    if(is.logical(t_last))  {
        t_last  <- lubridate::today() -1
    }
    
    log_trace("schedule: checking t_last:{t_last}, scheduled:{scheduled_time}")

    if((t_last < lubridate::today()) && (t_now >= scheduled_time)) {
    
        log_info("schedule: triggering ...")

        last_schedule_run <- lubridate::floor_date(t_now, unit = "day")
        add_doc(type = "schedule", ref = "last_run", doc = last_schedule_run,
                overwrite = TRUE, conn = pool)
        return(TRUE)

    } else {
        return(FALSE)
    }
}

