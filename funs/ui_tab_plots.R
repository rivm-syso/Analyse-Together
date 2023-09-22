## ------------------------
## Create content tabset plots
## -----------------------

tpPlots <- function(pick_plot = "timeplot"){
  if(pick_plot == "timeplot"){
    tpTimeplot()}
    else{
      tpBarplot()
      }
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
