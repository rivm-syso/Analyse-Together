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

# logger
library(logger)
log_threshold(TRACE)

# set data location
library(datafile)
datafileInit()

# load  dev version samanapir
remotes::install_github("rivm-syso/samanapir", ref = "Issue_2")
library(samanapir)


# load ATdatabase
remotes::install_github("rivm-syso/ATdatabase", ref = "develop",
                        build_opts ="")
library(ATdatabase)

# Set language and date options                                             ====

options(encoding = "UTF-8")                  # Standard UTF-8 encoding
Sys.setlocale("LC_TIME", 'dutch')            # Dutch date format
Sys.setlocale('LC_CTYPE', 'en_US.UTF-8')     # Dutch CTYPE format


# Connect with the database using pool                                      ====
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


# Read out the database to dataframes
measurements <- tbl(pool, "measurements") %>% as.data.frame()
location <- tbl(pool, "location") %>% as.data.frame()


