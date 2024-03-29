# Script to analyse the duration of the data requests as stored in the meta table in dbs
# Read in the necessary libraries                                           ====

# Tidyverse (essential)
library(tidyverse)

# Databases (essential)
library(RSQLite)
library(pool)

library(lubridate)
library(ATdatabase)
library(here)

# set working directory to root of repo
setwd(here::here())

# source scripts
source(here::here("funs","database_fun.R"))

# Connect with the database using pool, store data, read table            ====
fname_db <- get_database_path()
pool <- dbPool(

  drv = SQLite(),
  dbname = fname_db

)

# Get the meta table from the dbs                                     ====
meta_table <- tbl(pool, "meta") %>% as.data.frame()
# Select only the info about the data requests
data_requests <- meta_table %>% dplyr::filter(type == 'data_req_done')

# Create df to store the info
info_req_all <- data.frame(count_stations = numeric(), time_period = numeric(), time_req  = numeric())

# Gather for each request the number of stations, the timeperiod and the loading time
for (ind in seq(1,nrow(data_requests))){
  request <- data_requests[ind,]
  # Set from JSOn to df
  request_df <- request$doc[[1]] %>% jsonlite::fromJSON()
  # How many stations aer in thes request?
  count_stations <- request_df[[1]] %>% nrow()
  # Which time period was selected?
  time_start <-request_df[[1]]$time_start[1]
  time_end <- request_df[[1]]$time_end[1]
  time_period <- difftime(as.POSIXct(time_end), as.POSIXct(time_start), units = "days") %>% as.numeric()
  # how long did it take?
  time_req <- request_df[[2]] %>% dplyr::filter(row.names(.) == "elapsed") %>% pull()

  # combine all
  info_req <- data.frame(count_stations = count_stations,
                         time_period = time_period,
                         time_req  = time_req)

  # Add to df for all request
  info_req_all <- info_req_all %>% dplyr::bind_rows(info_req)
}

# Some summaries                                                      ====
summary(info_req_all)
mean_values <- info_req_all %>% dplyr::summarise_all("mean")
median_values <- info_req_all %>% dplyr::summarise_all("median")

mean_values
median_values

# Visualisations of the loading time: Count vs time                         ====
p_time_count <- ggplot(info_req_all, aes(time_req/60, count_stations)) +
  geom_point() +
  geom_smooth(method='lm', se = F) +
  labs(title = "Per request request: loading time vs number stations" ,
       x = "Time [minutes]", y = "Number stations") +
  theme_bw()
p_time_count

# Visualisations of the loading time: time vs count                             ====
p_time_count <- ggplot(info_req_all %>% dplyr::filter(count_stations !=100), aes( count_stations, time_req/60)) +
  geom_point() +
  geom_smooth(method='lm', se = F) +
  labs(title = "Per request request: loading time vs number stations" ,
       y = "Time [minutes]", x = "Number stations") +
  theme_bw()
p_time_count

# Calc lineair relation
m <- lm(formula = time_req ~ count_stations,
        data = info_req_all %>% dplyr::filter(count_stations !=100))
m
