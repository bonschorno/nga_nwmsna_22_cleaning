# Title: Create final log file
# Author: Colin Walder
# Contact: colin.walder@reach-initiative.org

rm(list = ls())

library(tidyverse)
library(readxl)
source("scripts/fn_reformat_logfile.R")

# "Other" checks ----------------------------------------------------------

col_order <- c("uuid", "question.name", "issue",
               "changed", "old.value", "new.value")

# first file
other_checks_one <- read_xlsx("old_otherchecks/R_other_checks.xlsx") %>% 
  rename(old.value = old_value,
         question.name = question_name,
         new.value = new_value,
         changed = action)

other_checks_one <- other_checks_one[, col_order]

# second file
other_checks_two <- readxl::read_xlsx("old_otherchecks/R_other_checks_2.xlsx") %>% 
  rename(old.value = old_value,
         question.name = question_name,
         new.value = new_value,
         changed = action,
         uuid = `_uuid`)


other_checks_two <- other_checks_two[, col_order]

# third file
other_checks_three <- read_csv("new_otherchecks/other_checks_output.csv")

# no overlapping identifiers
uuid_one <- other_checks_one %>%
  pull(uuid)

uuid_two <- other_checks_two %>%
  pull(uuid)

uuid_three <- other_checks_three %>% 
  pull(uuid)

Reduce(intersect, list(uuid_one,uuid_two,uuid_three))

final_other_checks_log <- bind_rows(other_checks_one, other_checks_two, other_checks_three) %>% 
  mutate(changed = tolower(changed),
         changed = ifelse(changed == "change", "changed", changed))

rm(other_checks_one, other_checks_two, other_checks_three)

unique(final_other_checks_log$changed)

# Logical flags -----------------------------------------------------------

# first file
logical_checks_one <- read_xlsx("old_logicalchecks/Data_logical_checks_R.xlsx") %>% 
  select(-contains("parent")) %>% 
  rename(feedback = `AO comment`,
         issue = problem,
         changed = action)

logical_checks_one <- logical_checks_one[, col_order]

# second file
logical_checks_two <- read_xlsx("old_logicalchecks/Data_logical_checks_R_2.xlsx") %>% 
  select(-contains("parent")) %>% 
  rename(feedback = `AO comment`,
         issue = problem,
         changed = action)

logical_checks_two <- logical_checks_two[, col_order]

# third file
logical_checks_three <- read_csv("new_logicalchecks/logical_checks_output.csv")

unique(logical_checks_three$question.name)

# create separate datasets for each variable

income_df <- reformat_logfile(logical_checks_three, "lack_income_cope", "lack_income_cope/none") 

shelter_df <- reformat_logfile(logical_checks_three, "shelter_enclosure_issues", "shelter_enclosure_issues/no_issues")

nearest_health_care_df <- logical_checks_three %>% 
  filter(question.name == "nearest_health_care") %>% 
  select(uuid, question.name, new.value)

diff_or_shocks_df <- reformat_logfile(logical_checks_three, "diff_or_shocks", "diff_or_shocks/no_shocks")

water_df <- logical_checks_three %>% 
  filter(question.name == "main_source_water") %>% 
  select(uuid, main_source_water = new.value) %>% 
  dummy_cols(select_columns = 'main_source_water',
             split = ",",
             ignore_na = TRUE,
             remove_selected_columns = TRUE) %>% 
  mutate("main_source_water/bottle_water" = 0) %>% 
  pivot_longer(-uuid, names_to = "question.name", values_to = "new.value") %>% 
  mutate(question.name = gsub("main_source_water_", "main_source_water/", question.name)) %>% 
  filter(new.value == 1 | question.name == "main_source_water/bottle_water") %>% 
  mutate(old.value = ifelse(question.name == "main_source_water/bottle_water", 1, 0)) %>% 
  mutate(new.value = as.character(new.value))

logical_checks_three_formatted <- bind_rows(income_df, shelter_df, nearest_health_care_df, diff_or_shocks_df, water_df) %>% 
  mutate(old.value = as.character(old.value))

rm(income_df, shelter_df, nearest_health_care_df, diff_or_shocks_df, water_df)
rm(logical_checks_three)

final_logical_checks_log <- bind_rows(logical_checks_one, logical_checks_two, logical_checks_three_formatted) %>% 
  mutate(changed = tolower(changed),
         changed = ifelse(changed == "change" | changed == "chenage", "changed", changed))
  
# no overlapping identifiers
uuid_one_log <- logical_checks_one %>%
  pull(uuid)

uuid_two_log <- logical_checks_two %>%
  pull(uuid)

uuid_three_log <- logical_checks_three_formatted %>% 
  pull(uuid)

Reduce(intersect, list(uuid_one_log,uuid_two_log,uuid_three_log))

# create final log file
final_log <- bind_rows(final_other_checks_log, final_logical_checks_log)

final_log %>% 
  filter(question.name != "enumerator_id") %>% 
  nrow()

final_log %>% 
  group_by(question.name) %>% 
  count() %>% 
  arrange(desc(n))

# Note to self: outlier checks file is still missing.

write_csv(final_log, "final_output/final_logfile.csv")