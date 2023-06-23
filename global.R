### DEFAULT SETTINGS                                                        ====

# Here we define some defaults, like the version number we're in, the language
# that the application uses, and the sourcing of custom functions.

# Define the version of your application                                    ====
application_version <- "0.99.0"
install_github <- FALSE # we run into API rate limits

# Read in the necessary libraries                                           ====
# Tidyverse (essential)
library(tidyverse)

# Lubridate (essential)
library(lubridate)

# datawizard
library(datawizard)


# Shiny (essential)
library(shiny)
library(shinycssloaders)
library(shinyWidgets)
# library(shinytest)
library(shinyjs)
library(shinyalert)

# For the translation
library(shiny.i18n)

# File with translations
i18n <- Translator$new(translation_json_path = "./lang/translation.json")
i18n$set_translation_language("nl") # here you select the default translation to display

# Databases (essential)
library(RSQLite)
library(pool)

# Visualisation
library(leaflet)         # For maps
library(leaflet.extras)  # For maps
library(sp)              # For maps
library(DT)              # For tables
library(latex2exp)       # For titles in graphs
library(openair)         # For openair-plots

# Geo
library(sf)

# logger
library(logger)
#log_threshold(loglevel)

library(samanapir)
library(ATdatabase)

# Source functions
library(here)
source("funs/assign_color_stations.R")
source("funs/assign_linetype_stations.R")
source("funs/geoshaper_findlocations.R")
source("funs/database_fun.R")
source("funs/queue_fun.R")
source("funs/download_fun.R")
source("funs/data_to_tool_fun.R")
source("funs/logging_fun.R")
set_loglevel()

# launch queue manager
qm_script <- here::here("scripts","queue_manager.R")
system2("Rscript", qm_script, wait = FALSE)

# Set language and date options                                             ====
options(encoding = "UTF-8")                  # Standard UTF-8 encoding
Sys.setlocale("LC_TIME", 'dutch')            # Dutch date format
Sys.setlocale('LC_CTYPE', 'en_US.UTF-8')     # Dutch CTYPE format

# Set theme for plots                                                       ====
theme_plots <- theme_bw(base_size = 18) +
  theme(strip.text.x = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(face = "bold",color = "black", size = 16),
        axis.text.x = element_text(color = "black", size = 16, angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(color = "black", size = 16),
        text = element_text(family = 'sans'),
        title = element_text(color = "black", size = 16),
        legend.title = element_text(size = 16),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  )

# Connect with the database using pool, store data, read table              ====
db_path <- get_database_path()
log_info("opening database {db_path}")
pool <- dbPool(
               drv = SQLite(synchronous = "off"),
               dbname = db_path
)
pool::dbExecute(pool, "PRAGMA busy_timeout = 60000")


## Initiate some variables                                                 ====
# Default start and end time for the date picker
default_time <- list(start_time = lubridate::today() - days(10), end_time = lubridate::today())

# store lists with projects and municipalities
municipalities <- read_csv("./prepped_data/municipalities.csv", col_names = F, col_types = c("d", "c"))
projects <- read_csv("./prepped_data/projects.csv", col_types = c("c"))

# add_doc doesn't work, see ATdatabase #8
ATdatabase::add_doc("application", "municipalities", municipalities, conn = pool,
        overwrite = TRUE)
ATdatabase::add_doc("application", "projects", projects, conn = pool,
        overwrite = TRUE)

# Connections with the database tables
measurements_con <- tbl(pool, "measurements")
stations_con <- tbl(pool, "location")

# log_info("Database ready, contains {nrow(sensor)} locations/sensors and {nrow(measurements)} measurements")

# Define colors, line types,choices etc.                                   ====
# Colours for the sensors
col_cat <- list('#e17000','#007bc7','#673327','#39870c','#ffb612','#42145f','#777c00', '#94710a','#01689b','#f9e11e','#76d2b6','#d52b1e','#8fcae7','#ca005d','#275937','#f092cd')
# col_cat <- rev(col_cat) # the saturated colours first
col_default <- '#000000'
col_overload <- '#111111'

# Linetype for the reference stations
line_cat <- list('dashed', 'dotted', 'dotdash', 'longdash', 'twodash')
line_default <- 'solid'
line_overload <- 'dotted'

# First name of the group
group_name_default <- "groep_1"
# Default for no group
group_name_none <- ""

# Minimal sd value sensor pm10
uc_min_pm10 <- 8.5

# Minimal sd value sensor pm25
uc_min_pm25 <- 5.3

# Codes of KNMI stations
knmi_stations <- as.vector(t(as.matrix(read.table(file = "prepped_data/knmi_stations.txt"))))

# Connections with the database tables
measurements_con <- tbl(pool, "measurements")
stations_con <- tbl(pool, "location")

# log_info("Database ready, contains {nrow(sensor)} locations/sensors and {nrow(measurements)} measurements")


# Component choices
overview_component <- data.frame('component' = c("pm10","pm10_kal","pm25","pm25_kal"), 'label'=c("PM10","PM10 - calibrated","PM2.5" ,"PM2.5 - calibrated" ))
comp_choices = setNames(overview_component$component, overview_component$label)
proj_choices = sort(projects$project)
mun_choices  = sort(municipalities$X2)

overview_select_choices <- data.frame('type' = c("project","municipality"), 'label'=c("project","gemeente"))
select_choices = setNames(overview_select_choices$type, overview_select_choices$label)


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
source("modules/select_mun_or_proj.R")
source("modules/choose_mun_or_proj.R")

# Source modules for metadata
source("modules/add_metadata_param_tables.R")
source("modules/add_single_text_message.R")

# Source modules visualisation
source("modules/add_bar_plot.R")
source("modules/add_timeseries_plot.R")
source("modules/add_pollutionrose_plot.R")
source("modules/add_timevariation_weekly_plot.R")
source("modules/add_timevariation_daily_plot.R")
source("modules/add_individual_timeseries_plot.R")

# Source layout
source("modules/add_tabpanels.R")

# Source buttons
source("modules/set_groupname_button.R")
source("modules/download_api_button.R")
source("modules/get_data_button.R")
source("modules/download_to_pc.R")

# # Source que display
# source("modules/view_que.R")
#
# # Create the queue
# que <- task_q$new()
#
### THE END                                                                 ====
