######################################################################
# This scripts launches and monitors the que manager
######################################################################
######################################################################

# Read in the necessary libraries                                           ====

# Tidyverse (essential)
library(tidyverse)

# Databases (essential)
library(RSQLite)
library(pool)

library(lubridate)

# logger
library(logger)
log_threshold(TRACE)

library(samanapir)
library(ATdatabase)
library(here)

# set working directory to root of repo
setwd(here::here())

# source scripts

source(here("funs","database_fun.R"))
source(here("funs","queue_fun.R"))
source(here("funs","download_fun.R"))
source(here("scripts","test_functions.R"))

qm_script <- here("scripts","queue_manager.R")

system2("Rscript", qm_script, wait = FALSE)

for(i in 1:5) {
    source(here("scripts", "create_data_request.R"))
}

while(TRUE) {
    Sys.sleep(3600)
}
