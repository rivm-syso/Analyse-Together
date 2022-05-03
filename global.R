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

# Read out the database to dataframes
cache <- tbl(pool, "cache") %>% as.data.frame()
measurements <- tbl(pool, "measurements") %>% as.data.frame()
meta <- tbl(pool, "meta") %>% as.data.frame()
sensor <- tbl(pool, "sensor") %>% as.data.frame()
sqlite_sequence <- tbl(pool, "sqlite_sequence") %>% as.data.frame()

