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

    
    tabsetPanel(
        
        tabPanel(
        
        value = "Select data",
        title = HTML("Select data <strong> <span style = 'color: #b2d7ee; font-size: 13px'> BETA </span> </strong>"),
        
        
        fluidRow(
          
          column(6, class = "col-lg-6", wellPanel(project_or_mun_selection_output("proj_or_mun_select"),
                                                  choice_selection_output("choice_select"),style = "z-index: 10;",
                                                  date_range_output("select_date_range"),style = "z-index: 1000;")),
          column(6, wellPanel(show_map_output("map")))
        ),
        
        ),
        
        tabPanel(

          
          value = "Metadata",
          title = HTML("Metadata <strong> <span style = 'color: #b2d7ee; font-size: 13px'> BETA </span> </strong>"),
          
          
          fluidRow(
            
            column(12, class = "col-lg-6", wellPanel(metadata_output("meta_table")))

          ),
          
        ),
      
      tabPanel(

        value = "Visualise data",
        title = HTML("Visualise data <strong> <span style = 'color: #b2d7ee; font-size: 13px'> BETA </span> </strong>"),
        
        fluidRow(
          column(6, class = "col-lg-6", wellPanel(component_selection_output("select_component")))
        ),
        
        # Output: Tabset voor openair plots, zie voor de inhoud het script: tabPanels.R
        tabsetPanel(tpAnalyse(), id = "tabsanalyse"
        )
        
      
      )


    )),
    
    tabPanel(
      title = "Information tool",
      helpText("This tool was build for citizens. The code is open-source. "),
      h4("Toelichting"),
      p("More information on the projects can be found on the samenmeten dataportaal.",
        style = "font-size:12px"))


  )

)

