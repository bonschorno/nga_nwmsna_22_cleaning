# Title: Flagging outliers in numeric variables
# Author: Colin Walder
# Contact: colin.walder@reach-initiative.org

rm(list = ls())

library(tidyverse)
library(readxl)
library(janitor) 

data <- read_csv("pre_clean_data/pre_clean_data.csv")

dim(data)

# Flagging outliers -----------------------------------

outlier_checks_xls <- read_xlsx("outlierchecks/outliers_check.xlsx") %>% 
  slice(3:n())

# vector with logics
conditions_vect <- outlier_checks_xls %>% 
  pull(condition)

# vector with corresponding messages
message_vect <- outlier_checks_xls %>% 
  pull(message)

# vector with question names
question_vect <- outlier_checks_xls %>% 
  pull(name)

# create empty list
issue_list <- list()

# check if two vectors are equally long (otherwise the loop doesn't work)
identical(length(conditions_vect), length(message_vect))

# get uuids from existing log file to ignore them
if(file.exists("outlierchecks/outlierchecks_output.csv") == TRUE){
  
  uuid_to_ignore <- read_csv("outlierchecks/outlierchecks_output.csv") %>% 
    pull(uuid)
  
  data_to_be_checked <- data %>% 
    filter(!`_uuid` %in% uuid_to_ignore)

}else{
  
  data_to_be_checked <- data
}

# looping over logical statements
for (i in 1:length(conditions_vect)) {
  
  uuid_vect <- data_to_be_checked %>% 
    filter(eval(str2lang(conditions_vect[i]))) %>% 
    pull("_uuid")
  
  message_statement <- message_vect[i]
  question_statement <- question_vect[i]
  
  value_vect <- data_to_be_checked %>%
    filter(`_uuid` %in% uuid_vect) %>%
    pull(question_statement)
  
  df_problem <- data.frame(uuid  = uuid_vect,
                           question.name = rep(question_statement, length(uuid_vect)),
                           issue = rep(message_statement, length(uuid_vect)),
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

issue_data %>% 
  group_by(question.name) %>% 
  count()

# write to file
outlier_file <- issue_data %>% 
  select(-starts_with("check_"), -several_issues)

if(file.exists("outlierchecks/outlierchecks_output.csv") == TRUE){
  
  write_csv(outlier_file, "outlierchecks/outlierchecks_output.csv",
            append = TRUE)
} else {
  
  write_csv(outlier_file, "outlierchecks/outlierchecks_output.csv",
            append = FALSE)
}


# Manual checks -----------------------------------------------------------------------

options(scipen = 999)

numeric_vars <- data %>% 
  as.data.frame() %>% 
  select(where(is.numeric), -contains("_gps"), -location_id_face, -"_id", -"_index") %>% 
  summarise_all(n_distinct) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "unique_values") %>% 
  filter(unique_values > 3) %>% 
  pull(variable)

numeric_vars_exclude <- c("age_respondent", "sum_school_age_girls",
                          "sum_school_age_boys", "start_to_end")

numeric_vars_clean <- numeric_vars[!numeric_vars %in% numeric_vars_exclude]

for (variable in numeric_vars_clean) {
  output <- paste(variable, ":", quantile(data[[variable]], probs = 0.95, na.rm = TRUE))
  print(output)
}
