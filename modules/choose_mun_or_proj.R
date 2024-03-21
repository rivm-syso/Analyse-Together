###############################################
### pickerInput - select name project/municipality ###
###############################################

# This is a municipality selection module, to select the name of the project/municipality
######################################################################
# Output Module
######################################################################

choice_selection_output <- function(id) {

  ns <- NS(id)
  uiOutput(ns("choice_select"))

}

######################################################################
# Server Module
######################################################################

choice_selection_server <- function(id,
                                    data_other,
                                    mun_choices,
                                    proj_choices,
                                    pre_select ) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

      output$choice_select <- renderUI({

      # Get if a mun or a proj is selected
      proj_or_mun <- data_other$mun_or_proj

      if (is.null(proj_or_mun) == FALSE){

          if (proj_or_mun == "municipality" ){
              # Create the component picker with a list of possible choices
              tagList(

                pickerInput(
                  ns("choice_select"),
                  label    = i18n$t("sel_option"),
                  choices  = mun_choices,
                  selected = pre_select,
                  multiple = TRUE,
                  width = "500px",
                  options = pickerOptions(maxOptions = 1)
                )
              )}
          else{
                tagList(

                pickerInput(
                  ns("choice_select"),
                  label    = i18n$t("sel_option"),
                  choices  = proj_choices,
                  selected = pre_select,
                  multiple = TRUE,
                  width = "500px",
                  options = pickerOptions(maxOptions = 1)
                ))}}
    })

    observeEvent(input$choice_select,{

      data_other$name_munproj <- input$choice_select

    })


  })

}
