######################################################################
# data_prep: gets some example data to work with
######################################################################
#
# This script uses the samanapir package to download some example data
# and store this in the data directory
#
# This script assumes that you did run the setup.R script to install
# necessary packages etc.


library(magrittr)
library(dplyr)
library(tidyr)
library(samanapir)
library(datafile)
datafileInit()


# load the 'amersfoort' set. A small selection of sensor data measured
# in the city of Amersfoort, NL. This will take several minutes to
# complete
fname_raw <- "amersfoort_raw.rds"

if(!file.exists(datafile(fname_raw))) {
       amersfoort_raw  <- samanapir::GetSamenMetenAPI("project eq'Amersfoort'","20190909", "20190912")
       saveRDS(amersfoort_raw,datafile(fname_raw))
} else {
    amersfoort_raw <- readRDS(datafile(fname_raw))
}


amersfoort <- amersfoort_raw$metingen %>%
    select(kit_id, date = tijd, waarde, grootheid) %>%
    filter(grootheid == "pm10_kal" | grootheid == "pm25_kal") %>%
    pivot_wider(names_from = grootheid, values_from = waarde)
saveRDS(amersfoort, datafile("amersfoort_measurements.rds"))
saveRDS(amersfoort_raw$sensordata, datafile("amersfoort_sensors.rds"))


amersfoort_avg <- amersfoort %>%
    group_by(date) %>%
    summarise(n = n(),
              average_pm10 = mean(pm10_kal, na.rm = TRUE),
              average_pm25 = mean(pm25_kal, na.rm = TRUE),
              sd_pm10 = mean(pm10_kal, na.rm = TRUE),
              sd_pm25 = mean(pm25_kal, na.rm = TRUE)
              )
saveRDS(amersfoort_avg, datafile("amersfoort_averages.rds"))



