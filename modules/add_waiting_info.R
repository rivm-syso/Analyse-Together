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

      # Sometimes a jop_type is AND data_req AND data_req_done, check if only 1
      if(length(job_type) == 1){
        # Check if the job is still on the waiting list
        if(job_type == "data_req_done"){

          log_trace("mod waiting info: yes job is done {job_id_interest}")

          # Creates message with job info about the data availability
          text_for_footer <- "klaar"
          return(text_for_footer)

        }else{

          # overview data requests in queu, including waiting number
          data_reqs_queu <- meta_table %>%
            dplyr::filter(type == "data_req") %>%
            dplyr::mutate(waiting = row_number())

          # Check the position of job in the waiting list
          waiting_number <- data_reqs_queu %>%
            dplyr::filter(str_detect(ref, job_id_interest)) %>%
            dplyr::select(waiting) %>%
            pull()

          # Get amount of stations in the waiting line
          amount_in_line <- 0
          for(x in seq(1, nrow(data_reqs_queu))){
            job_in_line <- data_reqs_queu[x,]

            print(job_in_line)
            x_in_line <- ATdatabase::get_doc(job_in_line$type,
                                                  job_in_line$ref,
                                                  pool) %>%
              count()
            amount_in_line <- amount_in_line + x_in_line
          }

          # Check if line is changed, update the counter
          # This can be the case if multiple in line
          if(waiting_number < data_other$waiting_number){
            data_other$waiting_number <- waiting_number
            data_other$waiting_counter <- 0
          }

          # Calculate approx. waiting time
          waiting_time <- waiting_number * 30 - data_other$waiting_counter

          log_trace("mod waiting info: waiting number {waiting_number}")

          # writes in footer the position of the job in the waiting list
          text_for_footer <- paste0(i18n$t("expl_waiting_number"),
                                    waiting_number, "
                                     Geschatte tijd is: ", waiting_time, "
                                     minuten.")

          return(text_for_footer)
        }
      }

    }

    output$waiting_text <- renderUI({

      # Do this every minute, if there is a job in line
      invalidateLater(60000)

      if(data_other$waiting_number > 0){

        data_other$waiting_counter <- isolate(data_other$waiting_counter) + 1

        log_trace("mod waiting info: check if job is done ...")
        # Check if the job is still in line
        text_for_footer <- check_job_info(data_other = data_other,
                                          pool = pool)

        # Check if data is available, show pop up if ready
        if(text_for_footer == "klaar"){
          data_other$waiting_number <- 0
          data_other$waiting_counter <- 0

          shinyalert::shinyalert(title = i18n$t("word_ready"),
                     text = i18n$t("expl_waiting_success"),
                     type = "success")
        }

        # Show footer if job is still in line
        div(p(text_for_footer))
        }

    })

  })

}
