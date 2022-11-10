######################################################################
# creates an new empty database
######################################################################
# !!!WARNING!!!!
# this script removes an old database if it exists
######################################################################

library(RSQLite)
library(pool)
library(ATdatabase)
source(here::here("funs","database_fun.R"))


db.path <- get_database_path()

if(file.exists(db.path)) {
    unlink(db.path)
}

pool <- dbPool(

               drv = SQLite(),
               dbname = db.path

)

create_database_tables(pool)

poolClose(pool)


