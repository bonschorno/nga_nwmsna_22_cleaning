# Title: Flagging potentially erroneous text entries
# Author: Colin Walder
# Contact: colin.walder@reach-initiative.org

rm(list = ls())

library(tidyverse)
library(readxl)

data <- read_csv("pre_clean_data/pre_clean_data.csv")

# Checking "text" answers -----------------------------------------------

other_answers <- data %>% 
  select(starts_with("other_"), -c(other_hh_members, other_missing_id), location_id_face) %>% 
  names()

other_answers

# get uuids from already existing log files
other_checks_uuids_1 <- read_xlsx("old_otherchecks/R_other_checks.xlsx") %>% 
  filter(question_name != "enumerator_id") %>% 
  pull(uuid)

other_checks_uuids_2 <- read_xlsx("old_otherchecks/R_other_checks_2.xlsx") %>%
  filter(question_name != "enumerator_id") %>% 
  pull(`_uuid`)

previous_check_uuids <- c(other_checks_uuids_1, other_checks_uuids_2) %>% 
  unique()


# get uuids from existing log file to ignore them
if(file.exists("new_otherchecks/other_checks_output.csv") == TRUE){
  
  uuid_to_ignore <- read_csv("new_otherchecks/other_checks_output.csv") %>% 
    pull(uuid)
  
  data_to_be_checked <- data %>% 
    filter(!`_uuid` %in% uuid_to_ignore) %>% 
    filter(!`_uuid` %in% previous_check_uuids)
  
}else{
  
  data_to_be_checked <- data %>% 
    filter(!`_uuid` %in% previous_check_uuids)
}

oth_log <- other_answers %>% 
  lapply(function(x) {
    data_to_be_checked %>% 
      select("_uuid", !!sym(x)) %>% 
      filter(!is.na(!!sym(x))) %>%
      as.data.frame() %>% 
      mutate(question.name = x, 
             feedback = "NULL",
             changed = "NULL",
             new.value = "NULL", 
             issue = "Recode the answer with an pre-defined answer if applicable") %>%
      rename(old.value = !!sym(x),
             uuid = "_uuid") %>%
      arrange(old.value)}) %>%
  do.call(rbind,.)

# change column order
col_order <- c("uuid", "question.name", "issue",
               "feedback", "changed", "old.value", "new.value")
oth_log <- oth_log[, col_order]

#save file
if(file.exists("new_otherchecks/other_checks_output.csv") == TRUE){
  
  write_csv(oth_log, "new_otherchecks/other_checks_output.csv",
            append = TRUE)
} else {
  
  write_csv(oth_log, "new_otherchecks/other_checks_output.csv",
            append = FALSE)
}