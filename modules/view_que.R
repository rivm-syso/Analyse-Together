view_que_output <- function(id) {

  ns <- NS(id)
  tagList(
  actionButton(ns("view_que_button"), "view que"),
  dataTableOutput(ns("view_que_button"))
  )
  }


view_que_server <- function(id, que) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

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




    get_que <- eventReactive(input$view_que_button, {
      T
    })

      output$view_que_button <-

        renderDataTable({
          get_que()
          # get an overview of the jobs in the queue
          que_data <- simple_task_list()%>%
            as.data.frame()

          if(length(que_data>1)){
            try(datatable(que_data,
                          options = list(scrollX = TRUE)))

          }})

    })
}
