## ------------------------
## Create content tabset grouping and selecting stations
## -----------------------

tpGrouping <- function(){

  fluidRow(
    helpText(i18n$t("expl_group"), style = "margin-left: 20px; "),
    wellPanel(
      fluidRow(
        p(i18n$t("expl_add_sensor_to_group")),
        switch_group_output("switch_group")
      ),
      fluidRow(
        set_group_button_output("set_group_pushed"),
        br()
      ),
      fluidRow(
        rename_group_button_output("rename_group")
      )
    ),

    h4(i18n$t("title_expl"), style = "margin-left: 20px"),
    p(i18n$t("expl_group_expl"),
      style = "font-size:12px; margin-left: 20px")
  )

}
