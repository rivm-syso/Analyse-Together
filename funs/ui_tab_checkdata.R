## ------------------------
## Create content tabset check data and cut-off
## -----------------------

tpCheckdata <- function(){

    fluidRow(
      helpText(i18n$t("expl_indu_timeplot"), style = "margin-left: 20px; "),


    fluidRow(

      # Render individual timeseries plot.
      column(12,class = "col-lg-12",

               outlier_cutoff_output("select_cutoff"),
               individual_timeseries_map_output("indu_timeseries")),

    ),
    h4(i18n$t("title_expl"), style = "margin-left: 20px"),
    p(i18n$t("expl_indu_timeplot_expl"),
      style = "font-size:12px; margin-left: 20px")
  )


}
