###############################################
### Visualisation show plots  ###
###############################################

# This is a module to create a visualisation based on input from he user
######################################################################
# Output Module
######################################################################

show_plot_output <- function(id) {

  ns <- NS(id)

  uiOutput(ns("show_plot"))

}


######################################################################
# Server Module
######################################################################

show_plot_server <- function(id,
                             pick_plot) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    get_plot <- reactive({

      if(pick_plot() == "timeplot"){
          tpTimeplot()
      }else if(pick_plot() == "barplot"){
          tpBarplot()
      }else if(pick_plot() == "timevariation_weekly"){
        tpTimevariationWeekly()
      }else if(pick_plot() == "timevariation_daily"){
        tpTimevariationDaily()
      }else if(pick_plot() == "calender"){
        tpCalenderPlot()
      }else if(pick_plot() == "pollutionrose"){
        tpPollutionRose()
      }

    })

    output$show_plot <- renderUI({
      tagList(
        get_plot()
      )
    })


  })

}
