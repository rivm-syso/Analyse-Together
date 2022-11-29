######################################################################
# creates an new empty database
######################################################################
# !!!WARNING!!!!
# this script removes an old database if it exists
######################################################################

library(tidyverse)
library(RSQLite)
library(pool)
library(ATdatabase)
library(here)
source(here("funs","database_fun.R"))


db.path <- get_database_path()

if(file.exists(db.path)) {
    unlink(db.path)
}

pool <- dbPool(

               drv = SQLite(),
               dbname = db.path

)

create_database_tables(pool)

municipalities <- read_csv(here("prepped_data", "municipalities.csv"), 
                           col_names = F)
projects <- read_csv(here("prepped_data", "projects.csv"), 
                           col_names = F)

add_doc("application", "municipalities", municipalities, conn = pool,
        overwrite = TRUE)
add_doc("application", "projects", projects, conn = pool,
        overwrite = TRUE)


poolClose(pool)


