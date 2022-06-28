# Merge new logical file

rm(list = ls())

library(tidyverse)
library(readxl)
source("scripts/fn_rewrite_other_column.R")
source("scripts/fn_create_separate_df.R")
library(fastDummies)

new_logical_checks <- read_csv("new_logicalchecks/logical_checks_output.csv")

# question names
unique(new_logical_checks$question.name)

# check for duplicates
new_logical_checks %>% 
  group_by(uuid, question.name) %>% 
  count() %>% 
  filter(n > 1) 

# create separate datasets for each varaible

income_df <- create_separate_df(new_logical_checks, question_name = "lack_income_cope", 
                                column_to_zero = "lack_income_cope/none")

shelter_df <- create_separate_df(new_logical_checks, question_name = "shelter_enclosure_issues", 
                                 column_to_zero = "shelter_enclosure_issues/no_issues")

nearest_health_care_df <- new_logical_checks %>% 
  filter(question.name == "nearest_health_care") %>% 
  select(uuid, question.name, new.value)

diff_or_shocks_df <- create_separate_df(new_logical_checks, question_name = "diff_or_shocks",
                                        column_to_zero = "diff_or_shocks/no_shocks")

water_df <- new_logical_checks %>% 
  filter(question.name == "main_source_water") %>% 
  select(uuid, main_source_water = new.value) %>% 
  dummy_cols(select_columns = 'main_source_water',
             split = ",",
             ignore_na = TRUE,
             remove_selected_columns = TRUE) %>% 
  mutate(`main_source_water/bottle_water` = 0) %>% 
  pivot_longer(-uuid, names_to = "question.name", values_to = "new.value") %>% 
  mutate(question.name = gsub("main_source_water_", "main_source_water/", question.name)) %>% 
  mutate(new.value = as.character(new.value))

new_logical_checks_formatted <- bind_rows(income_df, shelter_df, nearest_health_care_df, diff_or_shocks_df, water_df)

rm(income_df, shelter_df, nearest_health_care_df, diff_or_shocks_df, water_df)
rm(new_logical_checks)

# extract question names
question_names <- new_logical_checks_formatted %>% 
  distinct(question.name) %>% 
  pull()

vars_to_ignore <- c("uuid", "nearest_health_care")

# prepare data
data_list <- list()

# Note: The loop throws out a warning which can be ignored since it's caused by the last line that
# changes columns from string to numeric variables

for (variable in question_names) {
  data_list[[variable]] <- new_logical_checks_formatted %>% 
    filter(question.name == {{variable}}) %>% 
    distinct(uuid, .keep_all = TRUE) %>% 
    ungroup() %>% 
    select(uuid, question.name, new.value) %>% 
    pivot_wider(uuid, names_from = "question.name", values_from = "new.value") %>% 
    mutate(across(-one_of(vars_to_ignore), as.numeric))
}

# Clean data --------------------------------------------------------------------

# read in raw data
data_kobo <- read_csv("final_output/temporary_files/data_kobo_updated_old_logicalchecks_temp.csv") 

# initialise updated kobo data
data_kobo_updated <- data_kobo

# loop over list elements to changes values in raw dataset and implement them in updated dataset
for (variable in question_names) {
  data_kobo_updated <- data_kobo_updated %>% 
    rows_upsert(data_list[[variable]])
}

data_kobo_updated["lack_income_cope"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "lack_income_cope/")
data_kobo_updated["shelter_enclosure_issues"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "shelter_enclosure_issues/")
data_kobo_updated["diff_or_shocks"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "diff_or_shocks/")
data_kobo_updated["main_source_water"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "main_source_water/")

# Check before/after ------------------------------------------------------

# for main_source_water
sample_id <- "1cc200b2-e660-4132-9067-bdc6ae47ae3a"

pre <- data_kobo %>% 
  filter(uuid == sample_id) %>% 
  select(contains("main_source_water"))

post <- data_kobo_updated %>% 
  filter(uuid == sample_id) %>% 
  select(contains("main_source_water"))

# for lack_income_cope
sample_id <-"c0ba9987-e909-4fb1-9aba-011c55b30fa6"

pre <- data_kobo %>% 
  filter(uuid == sample_id) %>% 
  select(contains("lack_income_cope"))

post <- data_kobo_updated %>% 
  filter(uuid == sample_id) %>% 
  select(contains("lack_income_cope"))

write_csv(data_kobo_updated, "final_output/temporary_files/data_kobo_updated_new_logicalchecks_temp.csv") 
