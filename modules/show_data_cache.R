###############################################
### Show data availablility cache database ###
###############################################

# This is a module which shows in a shinyalert, the data available in the
# cache data base, and buttons to use this data or to download remaining data
######################################################################
# Output Module
######################################################################

show_data_cache_output <- function(id) {

  ns <- NS(id)
  uiOutput(ns("show_data_cache"))


}

######################################################################
# Server Module
######################################################################

show_data_cache_server <- function(id,
                                   data_to_plot,
                                   use_data_btn = F,
                                   get_data_btn = T

) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$show_data_cache <- renderUI({
      tagList(
        plotOutput(ns("plot_cache")),
        textOutput(ns("missing_days"))

      )
    })

    observeEvent(data_to_plot,{
      output$plot_cache <- renderPlot({
        openair::calendarPlot(data_to_plot,
                              pollutant = "available",
                              cols = c( "#d95f02","#1b9e77"),
                              breaks = c(-0.2,0.8,1.2),
                              labels = c( "Afwezig", "Aanwezig"),
                              key.position = "top")
      })

      output$missing_days <- renderText({
        missing_days <- data_to_plot %>%
          dplyr::filter(available == F) %>%
          count()
        paste0("Er missen nog: ", missing_days, " dagen.")
      })
    })


  })
}
