######################################################################
# Test functions
######################################################################
#
# miscelanous functions for application and database testing
#
######################################################################


get_db_tables <- function(conn) {
    # read all database tables and return a list with dataframes

    dbtbls <- list(measurements = tbl(conn,"measurements") %>% collect() ,
                   sensor = tbl(conn, "location") %>% collect(),
                   meta = tbl(conn, "meta") %>% collect(),
                   cache = tbl(conn, "cache") %>% collect()
    )

    return(dbtbls)

}



get_rnd_station <- function(dbtables) {
    # this function creates a randowm set of a station and time range,
    # this can be used to test the download function
    #
    # the input of this function (dbtables) is the output of the
    # get_db_tables function

    stations <- dbtables$measurements %>% 
        filter(!str_detect(station, "KNMI"), !str_detect(station, "NL")) %>%
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
                first_day = first_day,
                last_day = last_day,
                time_start = time_range[1],
                time_end = time_range[2])
    return(res)
}


station_overview <- function(conn) {
    # This function returns ggplot object with for each station the
    # downloaded ranges.

    dbtables <- get_db_tables(conn)
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
    return(p)
}


simple_task_list <- function(lst = que$list_tasks()) {
    # This function simplifies the que task list into a tibble with
    # state, sensor en time start/end columns

    get_args <- function(arg, pos) {
        res <- arg[[pos]]
        res <- ifelse(is.null(res), NA, res)
        return(res)

    }

    t2 <- lst %>%
        rowwise(args) %>%
        select(state, args)  %>%
        mutate(sensor = get_args(args, 1)) %>%
        mutate(time_start = as_datetime(get_args(args, 2))) %>%
        mutate(time_end = as_datetime(get_args(args, 3))) %>%
        ungroup() %>%
        select(-args) %>%
        na.omit()

    return(t2)
}






