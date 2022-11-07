# Define UI
shinyUI(

  # We initiate the navbarpage with inputs
  navbarPage(

    # For the top two headers (white and pink one)
    tags$head(

      # Read in the styles.css file
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),

      # Background set to a neutral grey
      setBackgroundColor(color = "#f3f3f3"),

      # White header with logo
      tags$div(class = "header-white",

               tags$img(src = "images/Banner_2018.png", style = "height: 180px; width: 690px; margin-bottom: 10px;margin-top: 10px; margin-right: 10px")

      ),

      # Colored header with text
      tags$div(class = "header-color",

               tags$style(HTML("h1 {margin-bottom: -15px;")),

               tags$h1("Samen Analyseren"),
               tags$h4(paste("Version", application_version)),
               tags$p("")
      )

    ),

    id          = "navbar",
    windowTitle = "Samen Analyseren Tool",
    selected    = "Home",

    tabPanel(
      title = "Home",

      fluidRow(
        column(width = 2, wellPanel(
        shiny.i18n::usei18n(i18n),
          radioGroupButtons('selected_language', size = 'sm',justified = T,width = '100px',
                            label = i18n$t("sel_language"),
                          choices = i18n$get_languages()[!i18n$get_languages() %in% grep("tag", i18n$get_languages(), value = T)],
                          selected = i18n$get_key_translation()))),

        conditionalPanel(condition="input.second_order_tabs!='Select data'",
                         column(width = 2, wellPanel(update_data_button_output("update_data"))))),

      fluidRow(
        column(width = 6 ,


                        tabsetPanel(id = "second_order_tabs",

                          tabPanel(

                            value = "Select data",
                            title = HTML(paste0(i18n$t("title_selectdata")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> BETA </span> </strong>")),


                            fluidRow(

                              column(6, class = "col-lg-6", wellPanel(project_or_mun_selection_output("proj_or_mun_select"),
                                                                      choice_selection_output("choice_select"),style = "z-index: 10;",

                                                                      date_range_output("select_date_range"),style = "z-index: 1000;",
                                                                      download_api_button_output("dl_btn_pushed"),style = "z-index: 1000;"
                                                            )
                              ),
                              column(6, class = "col-lg-6", wellPanel(
                                view_que_output("view_que"))

                            )

                          )
                          ),

                          tabPanel(

                            value = "Metadata",
                            title = HTML("Metadata <strong> <span style = 'color: #b2d7ee; font-size: 13px'> BETA </span> </strong>"),


                            fluidRow(

                              column(12, class = "col-lg-12", wellPanel(metadata_output("meta_table")), inlineCSS(list("table" = "font-size: 13px")))

                            )

                          ),

                          tabPanel(

                            value = "Visualise data",
                            title = HTML(paste0(i18n$t("title_visualisedata")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> BETA </span> </strong>")),

                                         fluidRow(
                                           wellPanel(component_selection_output("select_component"))
                                         ,

                                         # Output: Tabset voor openair plots, zie voor de inhoud het script: add_tabpanels.R
                                         tabsetPanel(tpAnalyse(), id = "tabsanalyse")

                            )
                          )
                        )
                      ),
      conditionalPanel(condition="input.second_order_tabs!='Select data'", column(width = 6,  show_map_output("map")))

        )
      ),

    tabPanel(
      title = i18n$t("title_infotool"),
      helpText(HTML('&nbsp;'),i18n$t("expl_expltool")),
      h4(HTML('&nbsp;'),i18n$t("title_expl")),
      p(HTML('&nbsp;'),i18n$t("expl_moreinfo"),
        style = "font-size:12px"))

  )

)
