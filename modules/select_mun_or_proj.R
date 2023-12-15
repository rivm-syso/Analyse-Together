###############################################
### pickerInput - to choose type either project or municipality ###
###############################################

# This is a project selection module, to choose type either project or municipality
######################################################################
# Output Module
######################################################################

project_or_mun_selection_output <- function(id) {

  ns <- NS(id)
  uiOutput(ns("proj_or_mun_select"))

}


######################################################################
# Server Module
######################################################################

project_or_mun_selection_server <- function(id,
                                            data_other,
                                            select_choices,
                                            pre_select ) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$proj_or_mun_select <- renderUI({
      # Create the component picker with a list of possible choices
      tagList(

        radioButtons(
          ns("proj_or_mun_select"),
          label    = i18n$t("sel_basedon"),
          choices  = select_choices,
          selected = pre_select,
          width = '300px'
        )
      )
    })

    observeEvent(input$proj_or_mun_select,{

      data_other$mun_or_proj <- input$proj_or_mun_select

    })

  })

}
