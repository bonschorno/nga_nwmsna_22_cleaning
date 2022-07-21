# Title: Flagging potentially erroneous entries
# Author: Colin Walder
# Contact: colin.walder@reach-initiative.org

rm(list = ls())

library(tidyverse)
library(readxl)
library(janitor) # for detecting duplicates

data <- read_csv("pre_clean_data/pre_clean_data.csv")

dim(data)

# Flagging logical errors -----------------------------------

# Talking you through:
# 1) Make sure that logical_checks.xls has R-readable boolean operators (e.g. "==" instead of "=").
# 2) Read in logical checks and remove first two rows (as they're not relevant for now)
# 3) Create a vector with all relevant logical statements, problem statements, and question names
# 4) Create empty list to loop into
# 5) Create vector with uuid already in log file so that they're ignored in the loop
# 6) Loop over logical statements, transform data from list to dataset, and append to exisiting dataset

logical_checks_xls <- read_xlsx("new_logicalchecks/logical_checks_r.xlsx") %>% 
  slice(3:n())

# vector with logics
logical_statements_vect <- logical_checks_xls %>% 
  pull(condition)

# vector with corresponding problem statements
problems_vect <- logical_checks_xls %>% 
  pull(problem)

question_vect <- logical_checks_xls %>% 
  pull(name)

# create empty list
issue_list <- list()

# check if two vectors are equally long (otherwise the loop doesn't work)
identical(length(logical_statements_vect), length(problems_vect))

# get uuids from already existing log files
logical_checks_uuids_1 <- read_xlsx("old_logchecks/logical_checks_R.xlsx") %>% 
  pull(uuid)

logical_checks_uuids_2 <- read_xlsx("old_logchecks/logical_checks_R_2.xlsx") %>% 
  pull(uuid)

previous_log_uuids <- c(logical_checks_uuids_1, logical_checks_uuids_2) %>% 
  unique()


# get uuids from existing log file to ignore them
if(file.exists("new_logicalchecks/logical_checks_output.csv") == TRUE){
  
  uuid_to_ignore <- read_csv("new_logicalchecks/logical_checks_output.csv") %>% 
    pull(uuid)
  
  data_to_be_checked <- data %>% 
    filter(!`_uuid` %in% uuid_to_ignore) %>% 
    filter(!`_uuid` %in% previous_log_uuids)
  
}else{
  
  data_to_be_checked <- data %>% 
    filter(!`_uuid` %in% previous_log_uuids)
}

# looping over logical statements
for (i in 1:length(logical_statements_vect)) {
  
  uuid_vect <- data_to_be_checked %>% 
    filter(eval(str2lang(logical_statements_vect[i]))) %>% 
    pull("_uuid")
  
  problem_statement <- problems_vect[i]
  question_statement <- question_vect[i]
  
  value_vect <- data_to_be_checked %>%
    filter(`_uuid` %in% uuid_vect) %>%
    pull(question_statement)
  
  df_problem <- data.frame(uuid  = uuid_vect,
                           question.name = rep(question_statement, length(uuid_vect)),
                           issue = rep(problem_statement, length(uuid_vect)),
                           feedback = rep(NA, length(uuid_vect)),
                           changed = rep(NA, length(uuid_vect)),
                           old.value = as.character(value_vect),
                           new.value = rep(NA, length(uuid_vect)),
                           check_failed = rep(paste0("check",i), length(uuid_vect)),
                           check_failed_num = rep(i, length(uuid_vect)))
  
  issue_list[[i]] <- df_problem
  
}

# put everything into one dataset

issue_data <-  bind_rows(issue_list) %>% 
  group_by(uuid) %>% 
  add_count(name = "count") %>% 
  mutate(several_issues = ifelse(count > 1, 1, 0)) %>% 
  select(-count)

# write to file
log_file <- issue_data %>% 
  select(-starts_with("check_"), -several_issues, -starts_with("parent"))

if(file.exists("new_logicalchecks/logical_checks_output.csv") == TRUE){
  
  write_csv(log_file, "new_logicalchecks/logical_checks_output.csv",
            append = TRUE)
} else {
  
  write_csv(log_file, "new_logicalchecks/logical_checks_output.csv",
            append = FALSE)
}