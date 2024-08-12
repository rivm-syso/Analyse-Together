## ------------------------
## Create content tabset grouping and selecting stations
## -----------------------

tpGrouping <- function(){

  fluidRow(

    wellPanel(
      fluidRow(
        style = "margin-left: 20px;",
        column(width = 10,helpText(i18n$t("infotext_step1_select"))
              ),
        column(width = 2,
          info_button_output("text_step1" )
        ),
      ),
      fluidRow(
        style = "margin-left: 20px;",
        select_all_button_output("select_all")
      ),
      fluidRow(
        style = "margin-left: 20px;",
        p(single_text_output("text_selected_sensors")),
        p(i18n$t("expl_add_sensor_to_group")),
        switch_group_output("switch_group")
      ),
      fluidRow(
        style = "margin-left: 20px;",
        set_group_button_output("set_group_pushed")
      ),
      br(),
      fluidRow(
        style = "margin-left: 20px;",
        rename_group_button_output("rename_group")
      )
    )
  )

}
