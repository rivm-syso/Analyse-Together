## ---------------------------------------------------------
## Functions for creating UI for the plots
## ---------------------------------------------------------


tpTimeplot <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_timeseries"),
                         helpText(i18n$t("expl_timeplot"),
                                  style = "margin-left: 20px; "),
                         info_button_output("plot_timeseries"),
                         fluidRow(
                           # Render timeseries plot.
                           column(12,class = "col-lg-12",
                                  wellPanel(timeseries_output("timeseries_plot"),
                                            slider_zoom_output("slider_zoom"))),
                         )
  )

  return(tp)
}

tpBarplot <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_barplot"),
                         helpText(i18n$t("expl_barplot"),
                                  style = "margin-left: 20px;"),
                         info_button_output("plot_barplot"),
                         fluidRow(
                           # render barplot
                           column(12, class = "col-lg-12",
                                  wellPanel(barplot_output("barplot_plot")))
                         )
  )

  return(tp)
}


tpTimevariationWeekly <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_weekpattern"),
                         helpText(i18n$t("expl_overviewplot_weekly"),
                                  style = "margin-left: 20px;"),
                         info_button_output("plot_weekly"),
                         fluidRow(
                           # Render  plot.
                           column(12, class = "col-lg-12",
                                  wellPanel(timevar_weekly_output("timevar_plot_weekly"))),
                         )
  )

  return(tp)
}

tpTimevariationDaily <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_daypattern"),
                         helpText(i18n$t("expl_overviewplot_daily"),
                                  style = "margin-left: 20px;"),
                         info_button_output("plot_daily"),
                         fluidRow(
                           # Render  plot.
                           column(12,class = "col-lg-12",
                                  wellPanel(timevar_daily_output("timevar_plot_daily"))),
                         )
  )

  return(tp)
}





tpCalenderPlot <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_cal_plot"),
                         helpText(i18n$t("expl_calplot"),
                                  style = "margin-left: 20px;"),
                         info_button_output("plot_windcal"),
                         fluidRow(
                           # Render pollutionrose plot.
                           column(12, class = "col-lg-12",
                                  wellPanel(calender_output("calender_plot"))),
                         )
  )

  return(tp)
}

tpPollutionRose<- function(){
  tp <-  shiny::tabPanel(i18n$t("word_conc_rose"),
                         helpText(i18n$t("expl_concplot%"),
                                  style = "margin-left: 20px;"),
                         info_button_output("plot_conc_rose"),
                         fluidRow(
                           # Render pollutionrose plot.
                           column(12, class = "col-lg-12",
                                  wellPanel(pollrose_output("pollrose_plot"))),
                         )
  )

  return(tp)
}



tpMetadata <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_meta_table"),
                         helpText(i18n$t("expl_meta_table"),
                                  style = "margin-left: 20px; "),
                         info_button_output("plot_table"),
                         fluidRow(
                           # Render table
                           column(12,class = "col-lg-12",
                                  wellPanel(metadata_param_output("meta_param_table"))),

                         )
  )

  return(tp)
}
