###############################################
### Select all stations button ###
###############################################

# This is a module which selects all stations
######################################################################
# Output Module
######################################################################

select_all_button_output <- function(id) {

  ns <- NS(id)
  tagList(
    actionButton(ns("select_all"),
                 i18n$t("btn_select_all"),
                 style="background-color: #ffe9b7",
                 width = "200px",
                 icon = icon("object-group"))
  )
}

######################################################################
# Server Module
######################################################################

select_all_button_server <- function(id,
                              group_name,
                              data_stations,
                              col_select,
                              line_cat,
                              line_default,
                              line_overload) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # When the button is pushed then select all stations (incl. knmi + ref)
    observeEvent(input$select_all,{
      # Get all the stations names from the stations with data
      id_select <- data_stations$data %>% dplyr::pull(station) %>% unique()

      # Set them to selected
      data_stations$data <- change_state_to_selected(data_stations$data,
                                                     id_select,
                                                     group_name(),
                                                     col_select(),
                                                     line_cat,
                                                     line_default,
                                                     line_overload)

    })


  })
}
