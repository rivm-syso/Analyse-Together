###############################################
### Gheck job queu waiting ###
###############################################

# This is a module which checks the progression of the queu,
# the latest job request is been checked every minute and the progression
# is shown in a footer
######################################################################
# Output Module
######################################################################

waiting_info_output <- function(id) {

  ns <- NS(id)
  uiOutput(ns("waiting_text"))

}

######################################################################
# Server Module
######################################################################

waiting_info_server <- function(id,
                                pool,
                                data_other
                                ) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    check_job_info <- function(pool,
                               data_other){
      browser()
      # Get job_id
      job_id_interest <- data_other$job_id

      shiny::validate(need(!job_id_interest == 0, "no job for this session"))

      # get meta data table of the cache dbs
      meta_table <- tbl(pool, "meta") %>%
        as.data.frame()

      # get the job from de cache dbs
      job_type <- meta_table %>%
        dplyr::filter(str_detect(ref, job_id_interest)) %>%
        dplyr::select(type) %>%
        pull()

      # Check if the job is still on the waiting list
      if(job_type == "data_req_done"){
        # Creates message with job info about the data availability, popup?
        browser()
        text_for_footer <- "klaar"
        return(text_for_footer)

      }else{
        browser()

        # overview data requests in queu, including waiting number
        data_reqs_queu <- meta_table %>%
          dplyr::filter(type == "data_req") %>%
          dplyr::select(-c(doc)) %>%
          dplyr::mutate(waiting = row_number())

        # Check the position of job in the waiting list
        waiting_number <- data_reqs_queu %>%
          dplyr::filter(str_detect(ref, job_id_interest)) %>%
          dplyr::select(waiting) %>%
          pull()

        # writes in footer the position of the job in the waiting list
        text_for_footer <- paste0("wachten: ", waiting_number)


        return(text_for_footer)
      }

    }

    output$waiting_text <- renderUI({
      browser()
      # Do this every minute

      invalidateLater(10000)
      text_for_footer <- check_job_info(data_other = data_other,
                                        pool = pool)
      div(p(text_for_footer))
    })

  })

}
