## ------------------------
## Create content tabset grouping and selecting stations
## -----------------------

tpGrouping <- function(){

  fluidRow(
    helpText(i18n$t("expl_meta_table"), style = "margin-left: 20px; "),
     fluidRow(
       column(width = 3, set_group_button_output("set_group_pushed")),
       column(width = 8, offset = 1,
              p(i18n$t("expl_add_sensor_to_group")),
              single_text_output("name_group"))
     ),

     wellPanel(
       metadata_param_output("meta_param_table")
     ),

    h4(i18n$t("title_expl"), style = "margin-left: 20px"),
    p(i18n$t("expl_meta_table_expl"),
      style = "font-size:12px; margin-left: 20px")
  )

}
