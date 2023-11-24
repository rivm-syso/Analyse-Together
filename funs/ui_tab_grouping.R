## ------------------------
## Create content tabset grouping and selecting stations
## -----------------------

tpGrouping <- function(){

  fluidRow(
    helpText(i18n$t("expl_meta_table"), style = "margin-left: 20px; "),
    wellPanel(
      fluidRow(
        p(i18n$t("expl_add_sensor_to_group")),
        single_text_output("name_group")
      ),
      fluidRow(
        set_group_button_output("set_group_pushed"))
    ),

    h4(i18n$t("title_expl"), style = "margin-left: 20px"),
    p(i18n$t("expl_meta_table_expl"),
      style = "font-size:12px; margin-left: 20px")
  )

}
