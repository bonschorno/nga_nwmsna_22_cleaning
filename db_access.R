# Create to database

rm(list = ls())

library(DBI)
suppressPackageStartupMessages(library(dplyr))

connection <- dbConnect(RPostgres::Postgres(),
                  host = "localhost",
                  dbname = "postgres",
                  user = rstudioapi::askForPassword("username"),
                  password = rstudioapi::askForPassword("pw"))

# read in raw kobo_data
raw_kobo_data_csv <- readr::read_delim("raw_data/data_kobo_28_6_2022.csv", delim = ";")
# 
# kobo_formatted <- raw_kobo_data_csv %>% 
#   rename(uuid = `_uuid`) %>% 
#   #select(-c(start, end, `_submission_time`, today)) %>% 
#   mutate(across(where(is.character), ~ substr(.x, 1, 5))) %>% 
#   mutate(across(everything(), as.character))
  
# char_vars <- kobo_formatted %>% 
#   select(where(is.character))
# 
# lapply(char_vars, function(x) max(nchar(x), na.rm = TRUE))

dbWriteTable(connection, 
             'raw_kobo_data', 
             raw_kobo_data_csv,
             overwrite=TRUE,
             row.names=FALSE)

test <- dbReadTable(connection,
                    "raw_kobo_data")

# Disconnect to clean up the connection to the database.
dbDisconnect(connection)
