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

      # White header with logo
      tags$div(class = "header-white",

               tags$img(src = "images/Banner_2018.png",
                        style = "height: 120px;
                        width: 450px;
                        margin-bottom: 0px;
                        margin-top: 0px;
                        margin-right: 10px")

      ),

      # Colored header with text
      tags$div(class = "header-color",

               tags$style(HTML("h1 {margin-bottom: -15px;")),

               tags$h1("Samen Analyseren"),
               tags$h4(paste("Version", application_version),
                       style = "margin-bottom: -10px"),
               tags$p("")
      )

    ),


    id          = "navbar",
    windowTitle = "Samen Analyseren Tool",
    selected    = "Home",

    tabPanel(
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
      # Secret button to call the browser() function during testing
      fluidRow(
        actionButton("browser", "browser"),
        tags$script("$('#browser').hide();")
      ),

      fluidRow(
        column(width = 6,

           tabsetPanel(id = "second_order_tabs",

            tabPanel(

               value = "Start",
               title = HTML(paste0(i18n$t("title_start")," <strong><span style = 'color: #b2d7ee; font-size: 13px'> </span> </strong>")),


               fluidRow(

                 column(width = 12,
                        wellPanel(

                        # Text for the user
                          div(h3(i18n$t("tool_welcome")),
                              p(i18n$t("tool_welcome_1_expl")),
                              actionButton("to_visualise_tab", "klik voor visualisatie"),
                              actionButton("to_select_tab", "Selecteer zelf data"),

                              info_sensor_output("info_sensor")
                        )
                        )
                     )
               )

                  ),


              tabPanel(

                 value = "Visualise data",
                 title = HTML(paste0(i18n$t("title_visualisedata")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> </span> </strong>")),

                 fluidRow(column(12, class = "col-lg-12",
                          wellPanel(component_selection_output("select_component"),
                                    style = "margin-bottom: -10px"))
                          ,

                          # Output: Tabset voor openair plots, zie voor de inhoud het script: add_tabpanels.R
                          tabsetPanel(tpAnalyse(), id = "tabsanalyse")
                 )
               ),
            tabPanel(

              value = "Select data",
              title = HTML(paste0(i18n$t("title_selectdata")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> </span> </strong>")),


              fluidRow(

                column(width = 12,
                       wellPanel(
                         div(h3(i18n$t("tool_welcome")),
                             p(i18n$t("tool_welcome_1_expl")),
                             p(i18n$t("tool_welcome_2_expl")),
                             p(i18n$t("tool_welcome_3_expl")),
                             p(i18n$t("tool_welcome_4_expl")),
                             p(i18n$t("tool_welcome_5_expl")),

                             p(i18n$t("expl_link_to_samenmeten"),
                               a("samenmeten.rivm.nl", href ='https://samenmeten.rivm.nl/dataportaal/', target = 'blank'),
                               br(),i18n$t("expl_link_to_LML"),
                               a("luchtmeetnet.nl", href ='https://www.luchtmeetnet.nl/', target = 'blank'),
                               br(),i18n$t("expl_link_to_KNMI"),
                               a("knmi.nl", href ='https://www.knmi.nl/', target = 'blank'),
                               br(),i18n$t("expl_link_to_projecten"),
                               a("samenmeten.nl/projecten", href ='https://samenmeten.nl/projecten', target = 'blank'),
                               style = "font-size:13px"), style='text-align: left;margin-top: -10px;'))
                )
              )

            ),
               tabPanel(
                 value = "Advanced",
                 title = HTML(paste0(i18n$t("title_advanced")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> </span> </strong>")),

                 fluidRow(
                   wellPanel(h4(i18n$t("expl_download_to_pc")),
                             p(i18n$t("expl_download_to_pc_expl")),
                             download_pc_button_output("download_pc"))

                 )

               )
           )
        ),

        # Conditional panels for the right side of the page:

        conditionalPanel(condition="input.second_order_tabs=='Select data'",
                           column(width = 6,
                                  wellPanel(project_or_mun_selection_output("proj_or_mun_select"),
                                            choice_selection_output("choice_select"),
                                            date_range_output("select_date_range"),
                                            column(6, get_data_button_output("get_btn_pushed")),
                                            column(6, download_api_button_output("dl_btn_pushed")),
                                            single_text_output("text_data_available"),
                                            br(),
                                            single_text_output("text_check_visualisation"),
                                            br(),
                                            single_text_output("text_download_estimation")
                                  )

                           )

                         ),
        conditionalPanel(condition="input.second_order_tabs=='Visualise data' | input.second_order_tabs=='Start' ",
                         column(width = 6,
                                style = "margin-top: 30px;",
                                style = "margin-bottom: 30px;",
                                fluidRow(
                                  show_map_output("map")),
                                br(),
                                  fluidRow(
                                    column(width = 3, set_group_button_output("set_group_pushed")),
                                    column(width = 8, offset = 1,
                                           p(i18n$t("expl_add_sensor_to_group")),
                                           single_text_output("name_group")))

                         )
                        )
      )
    ),

    tabPanel(
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
    )

  )

)

