# Merge old logfiles

rm(list = ls())

library(tidyverse)

data_kobo <- read_csv("pre_clean_data/pre_clean_data.csv")

other_vars_to_ignore <- data_kobo %>% 
  select(starts_with("other_")) %>% 
  names()

all_vars_to_ignore <- c(other_vars_to_ignore, "uuid", "enumerator_id", 
                                   "location_id_face", "reason_why")

rm(data_kobo)

# first file
other_checks_one <- readxl::read_xlsx("old_otherchecks/R_other_checks.xlsx") %>% 
  rename(old.value = old_value,
         question.name = question_name,
         new.value = new_value,
         changed = action)

col_order <- c("uuid", "question.name", "issue",
               "changed", "old.value", "new.value")

other_checks_one <- other_checks_one[, col_order]

# second file
other_checks_two <- readxl::read_xlsx("old_otherchecks/R_other_checks_2.xlsx") %>% 
  rename(old.value = old_value,
         question.name = question_name,
         new.value = new_value,
         changed = action,
         uuid = `_uuid`)

other_checks_two <- other_checks_two[, col_order]

# combine files
other_checks_final <- bind_rows(other_checks_one, other_checks_two)

# a few duplicates present which will be deleted in the loop
duplicates_check <- other_checks_final %>% 
  group_by(uuid, question.name) %>% 
  add_count() %>% 
  filter(n > 1) %>% 
  arrange(uuid, question.name)

# no empty question names
empty_questionnames <- other_checks_final %>% 
  filter(is.na(question.name))

# Prepare data for merging ------------------------------------------------

vars_other_checks <- other_checks_final %>% 
  distinct(question.name) %>% 
  pull()

data_list <- list()

# Note: The loop throws out a warning which can be ignored since it's caused by the last line that
# changes columns from string to numeric variables

for (variable in vars_other_checks) {
  data_list[[variable]] <- other_checks_final %>% 
    filter(question.name == {{variable}}) %>% 
    distinct(uuid, .keep_all = TRUE) %>% # I checked the duplicates and since they were identical it's safe to delete them
    ungroup() %>% 
    select(uuid, question.name, new.value) %>% 
    pivot_wider(uuid, names_from = "question.name", values_from = "new.value") %>% 
    mutate(across(-one_of(all_vars_to_ignore), as.numeric))
}


# Update raw data --------------------------------------------------------------------

# read in raw data
data_kobo <- read_csv("pre_clean_data/pre_clean_data.csv") %>% 
  rename(uuid = "_uuid")

# initialise updated kobo data
data_kobo_updated <- data_kobo

# loop over list elements to changes values in raw dataset and implement them in updated dataset
for (variable in vars_other_checks) {
  data_kobo_updated <- data_kobo_updated %>% 
    rows_upsert(data_list[[variable]])
}

# Change string -----------------------------------------------------------

source("scripts/fn_rewrite_other_column.R")

data_kobo_updated["difficulty_type_hh"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "difficulty_type_hh/")
data_kobo_updated["vunerability_type_hh"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "vunerability_type_hh/")
data_kobo_updated["hh_situation"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "hh_situation/")
data_kobo_updated["reaseon_for_displacement"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "reaseon_for_displacement/")
data_kobo_updated["current_area_displaced"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "current_area_displaced/")
data_kobo_updated["source_of_income"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "source_of_income/")
data_kobo_updated["secondary_income"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "secondary_income/")
data_kobo_updated["challenges_faced_hh_needs"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "challenges_faced_hh_needs/")
data_kobo_updated["diff_or_shocks"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "diff_or_shocks/")
data_kobo_updated["covid_action_taken"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "covid_action_taken/")
data_kobo_updated["main_source_water"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "main_source_water/")
data_kobo_updated["problems_water_access"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "problems_water_access/")
data_kobo_updated["cope_lack_water"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "cope_lack_water/")
data_kobo_updated["problems_sanitation_facicility"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "problems_sanitation_facicility/")
data_kobo_updated["adaptation_to_sanitation"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "adaptation_to_sanitation/")
data_kobo_updated["access_law_enforement_authorities"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "access_law_enforement_authorities/")
data_kobo_updated["barriers_boys_faced"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "barriers_boys_faced/")
data_kobo_updated["barriers_girls_faced"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "barriers_girls_faced/")
data_kobo_updated["support_regular_learning"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "support_regular_learning/")
data_kobo_updated["hh_assitance_recieved"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "hh_assitance_recieved/")
data_kobo_updated["prefer_info_assistance"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "prefer_info_assistance/")

# Check before/after ------------------------------------------------------

sample_id <- "6645285e-eab9-4ac0-81a9-d2293cae7b0e"

pre <- data_kobo %>% 
  filter(uuid == sample_id) %>% 
  select(contains("current_area_displaced"), other_choose)

post <- data_kobo_updated %>% 
  filter(uuid == sample_id) %>% 
  select(contains("current_area_displaced"), other_choose)

write_csv(data_kobo_updated, "final_output/temporary_files/data_kobo_updated_old_otherchecks_temp.csv")
