## ---------------------------------------------------------
## Functions for creating UI for the plots
## ---------------------------------------------------------


tpTimeplot <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_timeseries"),
                         helpText(i18n$t("expl_timeplot"),
                                  style = "margin-left: 20px; "),
                         fluidRow(

                           # Render timeseries plot.
                           column(12,class = "col-lg-12",
                                  wellPanel(timeseries_output("timeseries_plot"))),

                         ),
                         h4(i18n$t("title_expl"), style = "margin-left: 20px"),
                         p(i18n$t("expl_timeplot_expl"),
                           style = "font-size:12px; margin-left: 20px")
  )

  return(tp)
}

tpBarplot <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_barplot"),
                         helpText(i18n$t("expl_barplot"),
                                  style = "margin-left: 20px;"),

                         fluidRow(

                           # render barplot
                           column(12, class = "col-lg-12",
                                  wellPanel(barplot_output("barplot_plot")))

                         ),
                         h4(i18n$t("title_expl"), style = "margin-left: 20px"),
                         p(i18n$t("expl_barplot_expl"),
                           style = "font-size:12px;margin-left: 20px")
  )

  return(tp)
}


tpTimevariationWeekly <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_weekpattern"),
                         helpText(i18n$t("expl_overviewplot_weekly"), style = "margin-left: 20px;"),

                         fluidRow(

                           # Render  plot.
                           column(12, class = "col-lg-12",
                                  wellPanel(timevar_weekly_output("timevar_plot_weekly"))),

                         ),

                         h4(i18n$t("title_expl"), style = "margin-left: 20px"),
                         p(i18n$t("expl_overviewplot_expl_weekly"),
                           style = "font-size:12px;margin-left: 20px")
  )

  return(tp)
}

tpTimevariationDaily <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_daypattern"),
                         helpText(i18n$t("expl_overviewplot_daily"),
                                  style = "margin-left: 20px;"),

                         fluidRow(

                           # Render  plot.
                           column(12,class = "col-lg-12",
                                  wellPanel(timevar_daily_output("timevar_plot_daily"))),

                         ),

                         h4(i18n$t("title_expl"), style = "margin-left: 20px"),
                         p(i18n$t("expl_overviewplot_expl_daily"),
                           style = "font-size:12px;margin-left: 20px")
  )

  return(tp)
}





tpCalenderPlot <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_cal_plot"),
                         helpText(i18n$t("expl_calplot"), style = "margin-left: 20px;"),

                         fluidRow(

                           # Render pollutionrose plot.

                           column(12, class = "col-lg-12",
                                  wellPanel(calender_output("calender_plot"))),

                         ),
                         h4(i18n$t("title_expl"), style = "margin-left: 20px"),
                         p(i18n$t("expl_calplot_expl"),
                           style = "font-size:12px; margin-left: 20px")
  )

  return(tp)
}

tpPollutionRose<- function(){
  tp <-  shiny::tabPanel(i18n$t("word_conc_rose"),
                         helpText(i18n$t("expl_concplot%"), style = "margin-left: 20px;"),

                         fluidRow(

                           # Render pollutionrose plot.

                           column(12, class = "col-lg-12",
                                  wellPanel(pollrose_output("pollrose_plot"))),

                         ),
                         h4(i18n$t("title_expl"), style = "margin-left: 20px"),
                         p(i18n$t("expl_concplot%_expl"),
                           style = "font-size:12px; margin-left: 20px")
  )

  return(tp)
}


tpInduTimeplot <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_indu_timeseries"),
                         helpText(i18n$t("expl_indu_timeplot"), style = "margin-left: 20px; "),
                         fluidRow(

                           # Render individual timeseries plot.
                           column(12,class = "col-lg-12",
                                  wellPanel(individual_timeseries_output("indu_timeseries"))),

                         ),
                         h4(i18n$t("title_expl"), style = "margin-left: 20px"),
                         p(i18n$t("expl_indu_timeplot_expl"),
                           style = "font-size:12px; margin-left: 20px")
  )

  return(tp)
}


tpMetadata <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_meta_table"),
                         helpText(i18n$t("expl_meta_table"), style = "margin-left: 20px; "),
                         fluidRow(

                           # Render table
                           column(12,class = "col-lg-12",
                                  wellPanel(metadata_param_output("meta_param_table"))),

                         ),
                         h4(i18n$t("title_expl"), style = "margin-left: 20px"),
                         p(i18n$t("expl_meta_table_expl"),
                           style = "font-size:12px; margin-left: 20px")
  )

  return(tp)
}

tpInfotext <- function(){
  tp <-  shiny::tabPanel(i18n$t("word_info"),
                         fluidRow(

                           # Render table
                           column(12,class = "col-lg-12",
                                  wellPanel(p(i18n$t("infotext_vis_1_expl")),
                                            p(i18n$t("infotext_vis_2_expl"))))

                         )
  )

  return(tp)
}

tpMapplot <- function(){
  tp <-  shiny::tabPanel("Mooie kaart",
                         helpText("Uitleg van de kaart", style = "margin-left: 20px; "),
                         fluidRow(

                           # Render map
                           column(12,class = "col-lg-12",
                                  show_map_no_select_output("map_no_select_step3")


                         )
                         ),
                         h4("Zomaar wat tekst", style = "margin-left: 20px"),
                         p("zomaar wat tekst",
                           style = "font-size:12px; margin-left: 20px")
  )

  return(tp)
}
