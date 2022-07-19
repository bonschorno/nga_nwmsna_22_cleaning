# Title: Downloading data from Kobo
# Author: Colin Walder
# Contact: colin.walder@reach-initiative.org

rm(list = ls())
options(scipen = 999)

library(KoboconnectR)
library(httr)
library(tidyverse)
library(lubridate)
library(DBI)

# Download data from Kobo -------------------------------------------------

# credentials
#get_kobo_token(url="kobo.humanitarianresponse.info", uname=user_name, pwd=password)
#kobotools_api(uname = user_name, pwd = password)

# all available files
# overview <- kobo_exports(url="kobo.humanitarianresponse.info", uname=user_name, pwd=password)

# create export URL for GET request
new_export_url<- kobo_export_create(url="kobo.humanitarianresponse.info",
                                    uname=rstudioapi::askForPassword("username"),
                                    pwd=rstudioapi::askForPassword("password"),
                                    assetid="afQibyWMd8En8dPUuc9k37", 
                                    type= "csv", 
                                    all="false", 
                                    lang="_default",
                                    hierarchy="false",
                                    include_grp="true",
                                    grp_sep="/")

new_export_url

# get the data and store it locally

# date
date_today <- paste0(day(today()), "_", month(today()), "_", year(today()))

# wait a little (30-60s for Kobo to process the URL request)
df <- GET(new_export_url, authenticate(user=rstudioapi::askForPassword("username"), password =rstudioapi::askForPassword("password"))) 
df_content <- content(df, type="raw",encoding = "UTF-8") 

# path for most recent file
# most_recent_kobo_data <- paste0("raw_data/data_kobo_", date_today, ".csv")
# writeBin(df_content, most_recent_kobo_data)

writeBin(df_content, "raw_data/raw_kobo_data.csv")

rm(df_content, df)

# Write to Postgres database ----------------------------------------------

raw_kobo_data_csv <- read_delim("raw_data/raw_kobo_data.csv", delim = ";")

connection <- dbConnect(RPostgres::Postgres(),
                        host = "localhost",
                        dbname = "postgres",
                        user = rstudioapi::askForPassword("username"),
                        password = rstudioapi::askForPassword("pw"))

dbWriteTable(connection, 
             'raw_kobo_data', 
             raw_kobo_data_csv,
             overwrite=TRUE,
             row.names=FALSE)

dbDisconnect(connection)

# Create log_deletion file ------------------------------------------------

if(file.exists("final_output/final_deletionfile.csv") == TRUE){
  
  existing_deletion_ids <- read_csv("final_output/final_deletionfile.csv") %>% 
    pull(`_uuid`)
    
  data_raw <- read_delim("raw_data/raw_kobo_data.csv", delim = ";") %>% 
    filter(!`_uuid` %in% existing_deletion_ids)
  
  
}else{
  
  data_raw <- read_delim("raw_data/raw_kobo_data.csv", delim = ";")
}

data_deletion <- data_raw %>% 
  mutate(age_respondent = ifelse(age_respondent > 1900, 2022-age_respondent, age_respondent)) %>% 
  mutate(start = ymd_hms(start),
         end = ymd_hms(end),
         start_to_end = as.numeric(end-start)) %>% 
  filter(age_respondent < 18 | age_respondent > 70 | consent == "no" | origin_nigeria_outside == "another_country" | start_to_end < 15) %>% 
  mutate(reason_deletion = case_when(age_respondent < 18 ~ "Respondent younger than 18",
                                     age_respondent > 70 ~ "Respondent older than 70",
                                     consent == "no" ~ "Respondent did not consent",
                                     origin_nigeria_outside == "another_country" ~ "Respondent not from Nigeria",
                                     start_to_end < 15 ~ "Survey duration less than 15mins")) %>%
  select(`_uuid`, reason_deletion)

if(file.exists("final_output/final_deletionfile.csv") == TRUE){
  
  write_csv(data_deletion, "final_output/final_deletionfile.csv",append = TRUE)
  
}else{
  
  write_csv(data_deletion, "final_output/final_deletionfile.csv", append = FALSE)
}


# Create pre-cleaned data -------------------------------------------------

log_deletion_ids <- data_deletion %>% 
  pull(`_uuid`)

data_pre_clean <- data_raw %>% 
  filter(!`_uuid` %in% log_deletion_ids) %>% 
  mutate(age_respondent = ifelse(age_respondent > 1900, 2022-age_respondent, age_respondent),
         start = ymd_hms(start),
         end = ymd_hms(end),
         start_to_end = as.numeric(end-start))

# check time
quantile(data_pre_clean$start_to_end)

# check age criteria
quantile(data_pre_clean$age_respondent)

# check consent
data_pre_clean %>% 
  count(consent)

# check origin
data_pre_clean %>% 
  group_by(origin_nigeria_outside) %>% 
  count()

write_csv(data_pre_clean, "pre_clean_data/pre_clean_data.csv")


# Visualize n surveys per day -----------------------------------------------------

data_pre_clean %>% 
  filter(today > as.Date("2021-01-01")) %>% 
  group_by(today) %>% 
  count() %>% 
  arrange(desc(today)) %>% 
  view() %>% 
  ggplot(aes(x = today, y = n)) +
  geom_col()
