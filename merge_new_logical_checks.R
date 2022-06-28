# Transform new logical flags to merge with final dataset

rm(list = ls())

library(tidyverse)
library(fastDummies)

logical_checks_raw <- read_csv("new_logicalchecks/logical_checks_output.csv")

# transform multiple reponses to dummies

log_data %>% 
  filter(question.name == "main_source_water") %>% 
  pull(old.value)

pre_clean_data <- read_csv("pre_clean_data/pre_clean_data.csv")

test_df <- pre_clean_data %>% 
  select("_uuid", diff_or_shocks)

test_dummies <- dummy_cols(test_df, 
                           select_columns = 'diff_or_shocks',
                           split = " ",
                           ignore_na = TRUE) %>% 
  rename_with(~ tolower(gsub("diff_or_shocks_", "diff_or_shocks/", .x, fixed = TRUE))) %>% 
  names()

test_dummies <- sort(test_dummies)


location_id_df <- pre_clean_data %>% 
  select("_uuid", contains("diff_")) %>% 
  names()

location_id_df <- sort(location_id_df)

setequal(test_dummies, location_id_df)

names(test_dummies)
names(location_id_df)
