# Title: Checks for displacement years
# Author: Colin Walder
# Contact: colin.walder@reach-initiative.org

rm(list = ls())

library(tidyverse)
library(readxl)
library(janitor) 

data <- read_csv("pre_clean_data/pre_clean_data.csv")

# get uuids from existing log file to ignore them
if(file.exists("year_displ_checks/checks_output.csv") == TRUE){
  
  uuid_to_ignore <- read_csv("year_displ_checks/checks_output.csv") %>% 
    pull(uuid)
  
  data_to_be_checked <- data %>% 
    filter(!`_uuid` %in% uuid_to_ignore) 
  
}else{
  
  data_to_be_checked <- data 
  
}

checks_displacement_year <- data_to_be_checked %>% 
  select(uuid = `_uuid`, contains("year_first_displacement")) %>% 
  drop_na(year_first_displacement) %>% 
  pivot_longer(-uuid, names_to = "question.name", values_to = "old.value") %>% 
  filter(old.value < 2000) %>% 
  mutate(issue = "The year indicated wasn't a four digit integer",
         feedback = NA,
         changed = NA,
         new.value = NA) %>% 
  mutate(new.value = ifelse(between(old.value, 0, 12), 2022-old.value, NA))


# change column order
col_order <- c("uuid", "question.name", "issue",
               "feedback", "changed", "old.value", "new.value")

checks_displacement_year <- checks_displacement_year[, col_order]

#save file
if(file.exists("year_displ_checks/checks_output.csv") == TRUE){
  
  write_csv(checks_displacement_year, "year_displ_checks/checks_output.csv",
            append = TRUE)
} else {
  
  write_csv(checks_displacement_year, "year_displ_checks/checks_output.csv",
            append = FALSE)
}
