### DEFAULT SETTINGS                                                        ====

# Here we define some defaults, like the version number we're in, the language
# that the application uses, and the sourcing of custom functions.

# Define the version of your application                                    ====
application_version <- "0.0.1"
install_github <- FALSE # we run into API rate limits

# Read in the necessary libraries                                           ====

# Tidyverse (essential)
library(tidyverse)

# Shiny (essential)
library(shiny)
library(shinycssloaders)
library(shinyWidgets)
# library(shinytest)

# Databases (essential)
library(RSQLite)
library(pool)

# Visualisation
library(leaflet)         # For maps
library(leaflet.extras)  # For maps
library(DT)              # For tables
library(plotly)          # For graphs
library(latex2exp)       # For titles in graphs

# Geo
library(sf)

library(lubridate)

# logger
library(logger)
log_threshold(TRACE)

# set data location
# 
if(install_github) {
    remotes::install_github("jspijker/datafile", build_opts ="")
}
library(datafile)
datafileInit()

# load  dev version samanapir, jspijker's fork (contains more loging
if(install_github) remotes::install_github("jspijker/samanapir", ref = "Issue_2")
#if(install_github) remotes::install_github("rivm-syso/samanapir", ref = "Issue_2")
library(samanapir)


# load ATdatabase
if(install_github) {
    remotes::install_github("rivm-syso/ATdatabase", ref = "develop",
                            build_opts ="")
}

library(ATdatabase)

# Set language and date options                                             ====

options(encoding = "UTF-8")                  # Standard UTF-8 encoding
Sys.setlocale("LC_TIME", 'dutch')            # Dutch date format
Sys.setlocale('LC_CTYPE', 'en_US.UTF-8')     # Dutch CTYPE format


# Connect with the database using pool, store data, read table              ====
pool <- dbPool(

  drv = SQLite(),
  dbname = datafile("database.db")

)

# store lists with projects and municipalities
municipalities <- read_csv("./prepped_data/municipalities.csv")
projects <- read_csv("./prepped_data/projects.csv")

# add_doc doesn't work, see ATdatabase #8
add_doc("application", "municipalities", municipalities, conn = pool, 
        overwrite = TRUE)
add_doc("application", "projects", projects, conn = pool, 
        overwrite = TRUE)

# Define colors, line types,choices etc.                                   ====
# Colours for the sensors
col_cat <- list('#ffb612','#42145f','#777c00','#007bc7','#673327','#e17000','#39870c', '#94710a','#01689b','#f9e11e','#76d2b6','#d52b1e','#8fcae7','#ca005d','#275937','#f092cd')
col_cat <- rev(col_cat) # the saturated colours first
col_default <- '#000000'
col_overload <- '#111111'

# Linetype for the reference stations
line_cat <- list('dashed', 'dotted', 'dotdash', 'longdash', 'twodash')
line_default <- 'solid'
line_overload <- 'dotted'

# Codes of KNMI stations
knmi_stations <- c(269, 209, 215, 225, 235, 240, 242, 248, 249, 251, 257, 258, 260, 267, 270, 273, 275, 277, 278, 279, 280, 283, 285, 286, 290, 308, 310, 312, 313, 315, 316, 319, 324, 330, 340, 343, 344, 348, 350, 356, 370, 375, 377, 380, 391)

measurements <- tbl(pool, "measurements") %>% as.data.frame() %>% mutate(date = lubridate::as_datetime(timestamp, tz = "Europe/Amsterdam"))
sensor <- tbl(pool, "location") %>% as.data.frame() %>% mutate(selected = F, col = col_default, linetype = line_default, station_type = "sensor")

log_info("Database ready, contains {nrow(sensor)} locations/sensors and {nrow(measurements)} measurements")

# Component choices
overview_component <- data.frame('component' = c(" ","pm10","pm10_kal","pm25","pm25_kal"), 'label'=c(" ", "PM10","PM10 - calibrated","PM2.5" ,"PM2.5 - calibrated" ))
comp_choices = setNames(overview_component$component, overview_component$label)


### APP SPECIFIC SETTINGS                                                   ====

# Source module for the communication
source("modules/communication_module.R")

# Source module for the date range selection
source("modules/select_date_range.R")
# Source module for the component selection
source("modules/select_component.R")
# Source module for the component selection
source("modules/show_map.R")


# Source modules selections
source("modules/select_date_range.R")
source("modules/select_component.R")

# Source modules visualisation
source("modules/add_barplot.R")
source("modules/plot_timeseries.R")

# Source functions
source("funs/assign_color_stations.R")
source("funs/assign_linetype_stations.R")

### THE END                                                                 ====

