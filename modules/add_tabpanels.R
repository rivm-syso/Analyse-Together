## ---------------------------------------------------------
## Create tabpages for each visualisation.
## ---------------------------------------------------------

tpAnalyse <- function(){
  tp <-  shiny::tabsetPanel(
                           tpTimeplot(),
                           tpBarplot(),
                           # tpKalender(),
                           tpTimevariation(),
                           #tpPercentileRose(),
                           tpPollutionRose(),
                           #tpWindRose(),

                         )


  return(tp)
}

tpTimeplot <- function(){
  tp <-  shiny::tabPanel("Timeseries",
                         helpText(i18n$t("expl_timeplot")),
                         fluidRow(

                           # Render timeseries plot.
                           column(12, wellPanel(timeseries_output("timeseries_plot"))),

                         ),
                         h4(i18n$t("title_expl")),
                         p(i18n$t("expl_timeplot_expl"),
                           style = "font-size:12px")
  )

  return(tp)
}

tpBarplot <- function(){
  tp <-  shiny::tabPanel("Barplot",
                         helpText(i18n$t("expl_barplot")),

                         fluidRow(

                             # check output communicatition module
                             column(12, wellPanel(barplot_output("barplot_plot")))

                           ),
                         h4(i18n$t("title_expl")),
                         p(i18n$t("expl_barplot_expl"),
                           style = "font-size:12px")
  )

  return(tp)
}

tpKalender <- function(){
  tp <-  shiny::tabPanel("Calender plot",
                         helpText("This graph shows ..."),
                         plotOutput("calendar"),
                         h4(i18n$t("title_expl")),
                         p("If you select ...", HTML("&mu;g/m<sup>3</sup>."),
                           "The colors are indicating ...",
                           style = "font-size:12px")

  )
  return(tp)
}

tpTimevariation <- function(){
  tp <-  shiny::tabPanel("Overview of measurements",
                         helpText(i18n$t("expl_overviewplot")),

                         fluidRow(

                           # Render timevariation plot.

                           column(12, wellPanel(timevar_output("timevar_plot"))),

                         ),

                         h4(i18n$t("title_expl")),
                         p(i18n$t("expl_overviewplot_expl"),
                           style = "font-size:12px")
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

tpPollutionRose<- function(){
  tp <-  shiny::tabPanel("Concentratieroos (%)",
                         helpText(i18n$t("expl_concplot%")),
                         p("LET OP: als het KNMI-station geen gegevens over de wind heeft, kan er geen concentratieroos (%) worden getoond. Check of er een windroos
                    voor dit KNMI-station is."),
                         fluidRow(

                           # Render pollutionrose plot.

                           column(12, wellPanel(pollrose_output("pollrose_plot"))),

                         ),
                         h4(i18n$t("title_expl")),
                         p(i18n$t("expl_concplot%_expl"),
                           style = "font-size:12px")
  )

  return(tp)
}
