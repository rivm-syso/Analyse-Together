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
                         helpText("Deze grafiek laat de tijdreeks van de sensor in vergelijking met het meetstation zien."),
                         fluidRow(
                           
                           # Render timeseries plot. 
                           column(12, wellPanel(timeseries_output("timeseries_plot"))),
                          
                         ),
                         h4("Toelichting"),
                         p("Als je één of meerdere sensoren aanklikt, zie je een tijdreeks van de uurlijkse sensorwaarden voor de geselecteerde periode. 
                      Deze waarden kan je vergeleken met metingen van het dichtsbijzijnde meetstation van het luchtmeetnet. 
                      Op de x-as zie je de geselecteerde tijdsperiode; op de y-as staat de concentratie PM10 of PM2,5 in ug/m3",
                           style = "font-size:12px")
  )
  
  return(tp)
} 

tpBarplot <- function(){
  tp <-  shiny::tabPanel("Barplot",
                         helpText("Graph shows the average of the selected stations and sensors. The average is calculated for the timerange as selected in the previous step.
                                   The white number in each bar, displays the number of measurements within this timeperiod. If this number is very low, the statistical power of
                                  the difference between stations/sensors is lower."),

                         fluidRow(

                             # check output communicatition module
                             column(12, wellPanel(barplot_output("barplot_plot")))

                           ),
                         h4("Information"),
                         p("If you select ...",
                           style = "font-size:12px")
  )
  
  return(tp)
} 

tpKalender <- function(){
  tp <-  shiny::tabPanel("Calender plot",
                         helpText("This graph shows ..."),
                         plotOutput("calendar"),
                         h4("Toelichting"),
                         p("If you select ...", HTML("&mu;g/m<sup>3</sup>."),
                           "The colors are indicating ...",
                           style = "font-size:12px")
                         
  )
  return(tp)
} 

tpTimevariation <- function(){
  tp <-  shiny::tabPanel("Overview of measurements",
                         helpText("This graph ..."),
                         
                         fluidRow(
                           
                           # Render timevariation plot. 
                           
                           column(12, wellPanel(timevar_output("timevar_plot"))),
                           
                         ),
                          
                         h4("Toelichting"),
                         p("If you select ...", 
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
                         helpText("Deze grafiek toont per windrichting de relatieve bijdrage aan de totale gemiddelde concentratie per sensor of sensorgroep."),
                         p("LET OP: als het KNMI-station geen gegevens over de wind heeft, kan er geen concentratieroos (%) worden getoond. Check of er een windroos 
                    voor dit KNMI-station is."),
                         fluidRow(
                           
                           # Render pollutionrose plot. 
                           
                           column(12, wellPanel(pollrose_output("pollrose_plot"))),
                           
                         ),
                         h4("Toelichting"),
                         p("Als je een sensor aanklikt, wordt een gewogen concentratieroos getoond. Deze berekent per windsector het aandeel (in %) van deze 
                        sector in de totale gemiddelde concentratie. De gemiddelde concentratie per sector wordt hiervoor gewogen naar hoe vaak deze 
                        windrichting voorkomt. Voorbeeld: als er linksonder een driehoek ligt op de grijze lijn met 15% met een grote lichtpaarse vulling 
                        en een hele kleine donkerpaarse vulling, betekent dit dat de wind zo'n 15% van de tijd uit het zuidwesten waait en grotedeels lage 
                        concentraties (lichtpaarse vulling) brengt en slechts af en toe een hoge concenetratie (donkerpaarse vulling).",
                           style = "font-size:12px")
  )
  
  return(tp)
} 