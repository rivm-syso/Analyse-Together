# Define UI
shinyUI(

  # We initiate the navbarpage with inputs
  navbarPage(

    # For the top two headers
    tags$head(
      # Read in the styles.css file
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),

      # Background set to a neutral grey
      setBackgroundColor(color = "#f3f3f3"),

      # Colored header with text
      tags$div(class = "header-color",

               tags$style(HTML("h1 {margin-bottom: -15px;")),

               tags$h1("Samen Analyseren"),
               tags$h4(paste("Version", application_version),
                       style = "margin-bottom: -10px"),
               tags$p("")
      )
    ), # end of tags$head

    id          = "navbar",
    windowTitle = "Samen Analyseren Tool",
    selected    = "Home",

    tabPanel( # tabpanel "HOME" ----
      title = "Home",

      fluidRow(column(width = 1, offset = 10,
                      shiny.i18n::usei18n(i18n),
                      radioGroupButtons('selected_language',
                                        size = 'sm',
                                        justified = T,
                                        width = '80px',
                                        label = NULL,
                                        choices = i18n$get_languages()[!i18n$get_languages() %in% grep("tag", i18n$get_languages(), value = T)],
                                        selected = i18n$get_key_translation(), direction = 'horizontal'),
                      align = "left",
                      style = "margin-bottom: -10px;",
                      style = "margin-right: 20px;",
                      style = "margin-top: -10px;")),
      fluidRow(
        # 'Show' time to keep the app activated (not visible)
        h1(
          textOutput("currentTime", container = span), style = "font-size:12px; text-align:right; color:#ffffff;"
        )
      ),

      tabsetPanel(
        id = "second_order_tabs",

        tabPanel(
          value = "Start",
          title = HTML(paste0(i18n$t("title_start")," <strong><span style = 'color: #b2d7ee; font-size: 13px'> </span> </strong>")),

          fluidRow(

            column(width = 4,
                   wellPanel(

                     # Text for the user
                     div(h3(i18n$t("tool_welcome")),
                         p(i18n$t("tool_welcome_1_expl")),
                         p(i18n$t("tool_welcome_2_expl")),

                         actionButton("to_visualise_tab", i18n$t("btn_figures")),
                         actionButton("to_select_tab", i18n$t("btn_own_data"))

                     )
                   )
            ),
            column(width = 4,
                   style = "margin-top: 8px;",
                   info_sensor_output("info_sensor")
                   ),

            column(width = 4,
                   style = "margin-top: 8px;",
                   show_map_no_output("map_start")
            )


          )
        ), # end of tabpanel "START"

        tabPanel(
          value = "Visualise data",
          title = HTML(paste0(i18n$t("title_visualisedata")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> </span> </strong>")),

          fluidRow(
            column(width = 6,
                   tabsetPanel(
                     tabPanel(
                       value = "stap1",
                       title = "stap1",
                       tpGrouping()
                     ),
                     tabPanel(
                       value = "stap2",
                       title = "stap2",
                       tpCheckdata()
                     ),
                     tabPanel(
                       value = "stap3",
                       title = "stap3",
                       plot_selection_output("select_plot"),
                       show_plot_output("show_plot")
                     )
                   )

                   ),
            column(width = 6,
                   style = "margin-top: 8px;",
                   show_map_output("map"))

          )
        ), # end of tabpanel "VISUALISE DATA"

        tabPanel(
          value = "Select data",
          title = HTML(paste0(i18n$t("title_selectdata")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> </span> </strong>")),

          fluidRow(

            column(width = 12,
                   wellPanel(
                     div(h3(i18n$t("tool_select")),
                         p(i18n$t("tool_select_1_expl")),
                         p(i18n$t("tool_select_2_expl")),
                         project_or_mun_selection_output("proj_or_mun_select"),
                         p(i18n$t("tool_select_3_expl")),
                         choice_selection_output("choice_select"),
                         p(i18n$t("tool_select_4_expl")),
                         date_range_output("select_date_range"),
                         p(i18n$t("tool_select_5_expl")),
                         component_selection_output("select_component"),
                         p(i18n$t("tool_select_6_expl")),
                         download_api_button_output("dl_btn_pushed"),
                         p(i18n$t("tool_select_7_expl")),
                         get_data_cache_output("get_data_dbs_button_start")
                     )

                   )
            )
          )
        ), # end tabpanel "SELECT DATA"

        tabPanel(
          value = "Advanced",
          title = HTML(paste0(i18n$t("title_advanced")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> </span> </strong>")),

          fluidRow(
            wellPanel(h4(i18n$t("expl_download_to_pc")),
                      p(i18n$t("expl_download_to_pc_expl")),
                      download_pc_button_output("download_pc"))

          )
        ) # end tabpanel "ADVANCED"

      ) # end of tabsetpanel "second_order"

    ), # end tabpanel "HOME"

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
        br(),
        i18n$t("expl_link_to_projecten"),
        a("samenmeten.nl/projecten", href ='https://samenmeten.nl/projecten', target = 'blank'),
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


    # ----
  ) # end of navbarpage

) # end of shinyui
