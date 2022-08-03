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

               tags$img(src = "images/Banner_2018.png", style = "height: 120px; width: 610px; margin-bottom: 10px;margin-top: 10px; margin-right: 10px")

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
    selected    = "Test1",

    navbarMenu(
      
      title = "Explorer",

      tabPanel(

        value = "Test1",
        title = HTML("Test1 <strong> <span style = 'color: #b2d7ee; font-size: 13px'> BETA </span> </strong>"),

        # First row with 'breadcrumb'
        fluidRow(

          column(12,

                 wellPanel(style = "background-color: #f3f3f3; box-shadow: none;",

                           HTML("<span style = 'font-size: 25px; font-weight: bold;'> Explorer &gt Test1 </span><br>"),


                 )

          ),

        ),
        
        fluidRow(
          
          column(12, class = "col-lg-6", wellPanel(project_or_mun_selection_output("proj_or_mun_select"),
                                                   choice_selection_output("choice_select"),style = "z-index: 10;")),
          
        ),
        
        fluidRow(
          column(6, class = "col-lg-6", wellPanel(component_selection_output("select_component"))),

          column(6, class = "col-lg-6", wellPanel(date_range_output("select_date_range"),style = "z-index: 1000;")),
          
    

        ),
        

        fluidRow(

          # check output communicatition module
          column(6, wellPanel(barplot_output("barplot_plot"))),
          column(6, wellPanel(show_map_output("map")))
          

        ),
        
        fluidRow(

          # Render timeseries plot. 

          column(12, wellPanel(timeseries_output("timeseries_plot"))),

        ),
        
        fluidRow(
          
          # Render pollutionrose plot. 
          
          column(12, wellPanel(pollrose_output("pollrose_plot"))),
          
        ),
        
        fluidRow(
          
          # Render timevariation plot. 
          
          column(12, wellPanel(timevar_output("timevar_plot"))),
          
        )


      )


    )

  )

)

