## ------------------------
## Create content information about the tool
## -----------------------

tpInfo <- function(){

  tabPanel( # tabpanel "INFORMATION" ----
            value = "Information",
            title = i18n$t("title_infotool"),
    h4(i18n$t("word_ATTool")),
    p(i18n$t("tool_intro_expl")),br(),
    h4(i18n$t("word_data")),
    p(i18n$t("tool_intro_data_expl")),br(),
    h4(i18n$t("word_cal_values")),
    p(i18n$t("tool_intro_cal_values_expl")),br(),
    h4(i18n$t("word_confident_interval")),
    p(i18n$t("tool_confident_interval_1_expl")),
    p(i18n$t("tool_confident_interval_2_expl")),
    p(i18n$t("tool_confident_interval_3_expl")),
    p(i18n$t("tool_confident_interval_4_expl")),br(),
    h4(i18n$t("word_maximum_value")),
    p(i18n$t("tool_maximum_value_1_expl")),
    br(),
    h4(i18n$t("word_variation_whisker")),
    p(i18n$t("tool_variation_whisker_1_expl")),
    p(i18n$t("tool_variation_whisker_2_expl")),
    p(i18n$t("tool_variation_whisker_3_expl")),br(),
    h4(i18n$t("word_opensource")),
    p(i18n$t("tool_intro_opensource_expl")), br(),
    h4(i18n$t("word_links")),
    p(i18n$t("expl_link_to_samenmeten"),
      a("samenmeten.rivm.nl", href ='https://samenmeten.rivm.nl/dataportaal/', target = 'blank'),
      br(), i18n$t("expl_link_to_samenmeten_info"),
      a("link", href ='https://samenmeten.nl/dataportaal/samen-analyseren-tool', target = 'blank'),
      br(), i18n$t("expl_link_github"),
      a("github", href ='https://github.com/rivm-syso/Analyse-Together', target = 'blank'),
      br(),i18n$t("expl_link_to_LML"),
      a("luchtmeetnet.nl", href ='https://www.luchtmeetnet.nl/', target = 'blank'),
      br(),i18n$t("expl_link_to_KNMI"),
      a("knmi.nl", href ='https://www.knmi.nl/', target = 'blank'),
      br(),i18n$t("expl_link_to_openair"),
      a("openair", href ='https://davidcarslaw.github.io/openair/', target = 'blank'),
      br(),
      i18n$t("expl_link_to_projecten"),
      a("samenmeten.nl/initiatieven", href ='https://www.samenmeten.nl/initiatieven', target = 'blank'),
      br(),
      i18n$t("expl_link_to_benb_artikel"),
      a("link", href ='https://www.mdpi.com/1424-8220/22/20/8053', target = 'blank'),
      br(),
      i18n$t("expl_link_to_kalibration"),
      a("link", href ='https://samenmeten.nl/dataportaal/kalibratie-van-fijnstofsensoren', target = 'blank'),
      br(),
      "Contact: ",
      a("link", href ='https://samenmeten.nl/contact', target = 'blank'))

  ) # end of tabpanel "INFORMATION"
}
