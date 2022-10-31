######################################################################
# creates an new empty database
######################################################################
# !!!WARNING!!!!
# this script removes an old database if it exists
######################################################################

library(RSQLite)
library(pool)
library(ATdatabase)
library(datafile)

datafileInit()

if(file.exists(datafile("database.db"))) {
    unlink(datafile("database.db"))
}

pool <- dbPool(

               drv = SQLite(),
               dbname = datafile("database.db")

)

create_database_tables(pool)

poolClose(pool)


