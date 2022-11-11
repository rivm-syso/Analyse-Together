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
               
               tags$img(src = "images/Banner_2018.png", style = "height: 120px; width: 450px; margin-bottom: 0px;margin-top: 0px; margin-right: 10px")
               
      ),
      
      # Colored header with text
      tags$div(class = "header-color",
               
               tags$style(HTML("h1 {margin-bottom: -15px;")),
               
               tags$h1("Samen Analyseren"),
               tags$h4(paste("Version", application_version), style = "margin-bottom: -10px"),
               tags$p("")
      )
      
    ),
    
    id          = "navbar",
    windowTitle = "Samen Analyseren Tool",
    selected    = "Home",
    
    tabPanel(
      title = "Home",
      
      fluidRow(column(width = 1, offset=10,
                      shiny.i18n::usei18n(i18n),
                      radioGroupButtons('selected_language', size = 'sm',justified = T,width = '80px',
                                        label = NULL,
                                        choices = i18n$get_languages()[!i18n$get_languages() %in% grep("tag", i18n$get_languages(), value = T)],
                                        selected = i18n$get_key_translation(), direction = 'horizontal'),
                      align = "left",
                      style = "margin-bottom: -10px;",
                      style = "margin-right: 20px;",
                      style = "margin-top: -10px;")),
      
      fluidRow(
        column(width = 6,
               
               
               tabsetPanel(id = "second_order_tabs",
                           
                           tabPanel(
                             
                             value = "Select data",
                             title = HTML(paste0(i18n$t("title_selectdata")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> BETA </span> </strong>")),
                             
                             
                             fluidRow(
                               
                               column(12, class = "col-lg-12", wellPanel(project_or_mun_selection_output("proj_or_mun_select"),
                                                                         choice_selection_output("choice_select"),
                                                                         
                                                                         date_range_output("select_date_range"),
                                                                         download_api_button_output("dl_btn_pushed")
                               )
                               )
                               # ,
                               # column(6, class = "col-lg-6", wellPanel(
                               #   view_que_output("view_que"))
                               #   
                               # )
                               
                             )
                           ),
                           
                           tabPanel(
                             
                             value = "Informatie over data",
                             title = HTML(paste0(i18n$t("title_metadata")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> BETA </span> </strong>")),
                             
                             
                             fluidRow(
                               column(12, class = "col-lg-12", wellPanel(metadata_output("meta_table")), inlineCSS(list("table" = "font-size: 13px"))),
                               
                             ),
                             
                           ),
                           
                           tabPanel(
                             
                             value = "Visualise data",
                             title = HTML(paste0(i18n$t("title_visualisedata")," <strong> <span style = 'color: #b2d7ee; font-size: 13px'> BETA </span> </strong>")),
                             
                             fluidRow(class = "col-lg-12",
                                      wellPanel(component_selection_output("select_component"), style = "margin-bottom: -10px")
                                      ,
                                      
                                      # Output: Tabset voor openair plots, zie voor de inhoud het script: add_tabpanels.R
                                      tabsetPanel(tpAnalyse(), id = "tabsanalyse")
                                      
                             )
                           )
               )
        ),
        
        # Add all text:
        
        conditionalPanel(condition="input.second_order_tabs!='Select data'",
                         column(width = 1, update_data_button_output("update_data"),                
                                align = "center", style = "margin-left: -10px;margin-top: -10px; margin-bottom: -10px; margin-right:-10px;")),
        conditionalPanel(condition="input.second_order_tabs=='Select data'", column(width = 6,div(br(),br(),h3(i18n$t("tool_welcome")),
                                                                                                  p(i18n$t("tool_welcome_expl")),
                                                                                                  p(i18n$t("link_to_samenmeten"), a("samenmeten.rivm.nl", href ='https://samenmeten.rivm.nl/dataportaal/', target = 'blank'),
                                                                                                    br(),i18n$t("link_to_LML"), a("luchtmeetnet.nl", href ='https://www.luchtmeetnet.nl/', target = 'blank'),
                                                                                                    br(),i18n$t("link_to_projecten"), a("samenmeten.nl", href ='https://samenmeten.nl/projecten', target = 'blank'), style = "font-size:13px"), style='text-align: left;margin-top: -10px;'))),
        conditionalPanel(condition="input.second_order_tabs=='Informatie over data'", column(width = 6,div(br(),i18n$t("expl_metadata")))),
        conditionalPanel(condition="input.second_order_tabs=='Visualise data'", column(width = 6,  show_map_output("map"),style = "margin-top: 30px;")),
        
      )
    ),
    
    tabPanel(
      title = i18n$t("title_infotool"),
      h4(i18n$t("title_expl")),
      p(i18n$t("tool_intro"),
        style = "font-size:13px"))
    
  )
  
)