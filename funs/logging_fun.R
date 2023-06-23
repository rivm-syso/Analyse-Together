set_loglevel <- function(level = "INFO") {

    if ("ANALYSETOGETHER_LOGLEVEL" %in% names(Sys.getenv())) {
        loglevel <- Sys.getenv("ANALYSETOGETHER_LOGLEVEL")
    } else {
        loglevel <- level
    }

    log_threshold(loglevel)
    log_warn(glue::glue("Loglevel set at {loglevel}"))

    invisible(level)
}
