### DEFAULT SETTINGS                                                        ====

# Here we define some defaults, like the version number we're in, the language
# that the application uses, and the sourcing of custom functions.

# Define the version of your application                                    ====
application_version <- "0.0.1"

# Read in the necessary libraries                                           ====

# Tidyverse (essential)
library(tidyverse)

# Shiny (essential)
library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(shinytest)

# Databases (essential)
library(RSQLite)
library(pool)

# Visualisation
library(highcharter)     # For charts
library(leaflet)         # For maps
library(DT)              # For tables
library(plotly)          # For graphs

# Geo
library(sf)

# set data location
library(datafile)
datafileInit()

# Set language and date options                                             ====

options(encoding = "UTF-8")                  # Standard UTF-8 encoding
Sys.setlocale("LC_TIME", 'dutch')            # Dutch date format
Sys.setlocale('LC_CTYPE', 'en_US.UTF-8')     # Dutch CTYPE format


# Connect with the database using pool                                      ====
pool <- dbPool(

  drv = SQLite(),
  dbname = datafile("database.db")

)
# Colours for the sensors
col_cat <- list('#ffb612','#42145f','#777c00','#007bc7','#673327','#e17000','#39870c', '#94710a','#01689b','#f9e11e','#76d2b6','#d52b1e','#8fcae7','#ca005d','#275937','#f092cd')
col_default <- '#000000'
col_overload <- '#111111'

# Linetype for the reference stations
line_cat <- list('dashed', 'dotted', 'dotdash', 'longdash', 'twodash')
line_default <- 'solid'
line_overload <- 'dotted'

# Read out the database to dataframes
measurements <- tbl(pool, "measurements") %>% as.data.frame() %>% mutate(date = lubridate::as_datetime(timestamp, tz = "Europe/Amsterdam"))
meta <- tbl(pool, "meta") %>% as.data.frame()
sensor <- tbl(pool, "sensor") %>% as.data.frame() %>% mutate(selected = F, col = col_default, linetype = line_default, station_type = "sensor")


# Colours for the sensors
col_cat <- list('#ffb612','#42145f','#777c00','#007bc7','#673327','#e17000','#39870c', '#94710a','#01689b','#f9e11e','#76d2b6','#d52b1e','#8fcae7','#ca005d','#275937','#f092cd')
col_cat <- rev(col_cat) # the saturated colours first

# Component choices
overview_component <- data.frame('component' = c(" ","pm10","pm10_kal","pm25","pm25_kal"), 'label'=c(" ","PM10","PM10 - gekalibreerd","PM2.5" ,"PM2.5 - gekalibreerd" ))
comp_choices = setNames(overview_component$component, overview_component$label)


### APP SPECIFIC SETTINGS                                                   ====

# Source module for the communication
source("modules/communication_module.R")

# Source modules selections
source("modules/select_date_range.R")
source("modules/select_component.R")

# Source modules visualisation

# Source functions
source("funs/assign_color_stations.R")
source("funs/assign_linetype_stations.R")
