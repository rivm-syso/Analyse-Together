## ---------------------------------------------------------
## Create tabpages for each visualisation.
## ---------------------------------------------------------

tpAnalyse <- function(){
  tp <-  shiny::tabsetPanel(
                           tpInfotext(),
                           tpMetadata(),
                           tpTimeplot(),
                           tpInduTimeplot(),
                           tpBarplot(),
                           tpTimevariationWeekly(),
                           tpTimevariationDaily(),
                           #tpPercentileRose(),
                           tpCalenderPlot(),
                           tpPollutionRose(),
                           #tpWindRose(),

                         )


  return(tp)
}

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

tpWindRose<- function(){
  tp <-  shiny::tabPanel("Windroos",
                         helpText("Deze grafiek toont per windrichting hoe vaak en hoe hard de wind waaide per sensor of sensorgroep."),
                         p("LET OP: als het KNMI-station geen gegevens over de wind heeft, kan er geen windroos worden getoond."),

                         plotOutput("windplot"),
                         div(style="padding-top:10px;",plotOutput("wind_timeplot")),
                         h4("Toelichting"),
                         p("Als je een sensor aanklikt, wordt een windroos getoond.
                        Deze windroos laat de windsnelheid en -richting zien van het dichtstbijzijnde KNMI-station. Voor elke windsector toont de
                        grafiek in hoeveel procent van de tijd de wind vanuit die richting waaide.
                        De gekleurde blokken geven de windsnelheid aan. Bijvoorbeeld: wanneer de wind voornamelijk uit het zuidwesten komt,
                        zie je de langste blokken linksonder (tussen zuid en west in). Als je wilt weten hoe hard de wind waaide,
                        bekijk je de kleur van de blokken. Hoe donkerder de kleur, hoe harder de wind.",
                           style = "font-size:12px")
  )

  return(tp)
}

tpPercentileRose<- function(){
  tp <-  shiny::tabPanel("Concentratieroos",
                         helpText("Deze grafiek toont de gemiddelde concentratie per windrichting per sensor of sensorgroep."),
                         p("LET OP: als het KNMI-station geen gegevens over de wind of de sensor alleen 0 ug/m3 gemeten heeft,
                    is deze concentratieroos vreemd. Check of er een windroos voor dit KNMI-station is. Check in de tijdreeks of
                    de sensor metingen boven de 0 ug/m3 heeft."),
                    plotOutput("percentileplot"),
                    h4("Toelichting"),
                    p("Als je een sensor aanklikt, wordt een concentratieroos getoond. Deze toont per windsector het gemiddelde
                        van de sensormetingen wanneer de wind uit die richting waaide. Voorbeeld: als aan de rechterbovenzijde
                        van de grafiek de grijze lijntjes op de streep voor 20 ug/m3 ligt en aan de linkerbovenzijde op 10 ", HTML("&mu;g/m<sup>3</sup>."),
                      "dan betekent dit dat bij wind van het noordoosten de concentraties hoger zijn dan bij wind vanuit het noordwesten.",
                      style = "font-size:12px"
                    )
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

