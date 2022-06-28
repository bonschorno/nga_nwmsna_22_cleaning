# Transform new "other checks" file to merge with final dataset

rm(list = ls())

library(tidyverse)

other_checks_new <- read_csv("new_otherchecks/other_checks_output.csv") %>% 
  mutate(new.value = ifelse(str_detect(question.name, "other_") & new.value == "0", NA, new.value))

# preliminary checks
duplicates_check <- other_checks_new %>% 
  group_by(uuid, question.name) %>% 
  add_count() %>% 
  filter(n > 1) %>% 
  arrange(uuid, question.name)

empty_questionnames_check <- other_checks_new %>% 
  filter(is.na(question.name))

# vars to ignore
data_kobo <- read_csv("pre_clean_data/pre_clean_data.csv")

other_vars_to_ignore <- data_kobo %>% 
  select(starts_with("other_")) %>% 
  names()

all_vars_to_ignore <- c(other_vars_to_ignore, "uuid", "enumerator_id", 
                        "location_id_face", "reason_why")

rm(data_kobo, duplicates_check, empty_questionnames_check)


# Prepare merge
vars_other_checks <- other_checks_new %>% 
  distinct(question.name) %>% 
  pull()

data_list <- list()

# Note: The loop throws out a warning which can be ignored since it's caused by the last line that
# changes columns from string to numeric variables

for (variable in vars_other_checks) {
  data_list[[variable]] <- other_checks_new %>% 
    filter(question.name == {{variable}}) %>% 
    distinct(uuid, .keep_all = TRUE) %>% # remove duplicates
    ungroup() %>% 
    select(uuid, question.name, new.value) %>% 
    pivot_wider(uuid, names_from = "question.name", values_from = "new.value") %>% 
    mutate(across(-one_of(all_vars_to_ignore), as.numeric))
}


# Clean data --------------------------------------------------------------------

# read in raw data
data_kobo <- read_csv("final_output/temporary_files/data_kobo_updated_old_otherchecks_temp.csv")

# initialise updated kobo data
data_kobo_updated <- data_kobo

# loop over list elements to changes values in raw dataset and implement them in updated dataset
for (variable in vars_other_checks) {
  data_kobo_updated <- data_kobo_updated %>% 
    rows_upsert(data_list[[variable]])
}

# Change string -----------------------------------------------------------

data_kobo_updated <- data_kobo_updated %>% 
  mutate(`barriers_boys_faced/other` = ifelse(is.na(other_barriers_boys) & `barriers_boys_faced/other` == 1, 0, `barriers_boys_faced/other`),
       `barriers_girls_faced/other` = ifelse(is.na(other_barriers_girls) & `barriers_girls_faced/other` == 1, 0, `barriers_girls_faced/other`),
       `challenges_faced_hh_needs/other` = ifelse(is.na(other_challenges_meet) & `challenges_faced_hh_needs/other` == 1, 0, `challenges_faced_hh_needs/other`),
       `current_area_displaced/other` = ifelse(is.na(other_choose) & `current_area_displaced/other` == 1, 0, `current_area_displaced/other`),
       `difficulty_type_hh/other` = ifelse(is.na(other_difficulties) & `difficulty_type_hh/other` == 1, 0, `difficulty_type_hh/other`),
#       `handwashing_facility/other` = ifelse(is.na(other_hand_washing_facility) & `handwashing_facility/other` == 1, 0, `handwashing_facility/other`),
       `hh_assitance_recieved/other` = ifelse(is.na(other_hh_assistance) & `hh_assitance_recieved/other` == 1, 0, `hh_assitance_recieved/other`),
       `source_of_income/other` = ifelse(is.na(other_income) & `source_of_income/other` == 1, 0, `source_of_income/other`),
       `secondary_income/other` = ifelse(is.na(other_income_secondary) & `secondary_income/other`  == 1, 0, `secondary_income/other`),
       `prefer_info_assistance/other` = ifelse(is.na(other_info_source) & `prefer_info_assistance/other` == 1, 0, `prefer_info_assistance/other`),
#       `main_language_home/other` = ifelse(is.na(other_language) & `main_language_home/other` == 1, 0, `main_language_home/other`),
#       `which_language/other` = ifelse(is.na(other_language_speak) & `which_language/other` == 1, 0, `which_language/other`),
       `access_law_enforement_authorities/other` = ifelse(is.na(other_law_enforce_auth) & `access_law_enforement_authorities/other` == 1, 0, `access_law_enforement_authorities/other`),
       `reaseon_for_displacement/other` = ifelse(is.na(other_reasons) & `reaseon_for_displacement/other` == 1, 0, `reaseon_for_displacement/other`),
       `support_regular_learning/other` = ifelse(is.na(other_regular_learning) & `support_regular_learning/other` == 1, 0, `support_regular_learning/other`),
       `adaptation_to_sanitation/other` = ifelse(is.na(other_sanitation_adaptation) &`adaptation_to_sanitation/other` == 1, 0, `adaptation_to_sanitation/other`),
       `problems_sanitation_facicility/other` = ifelse(is.na(other_sanitation_problem) & `problems_sanitation_facicility/other` == 1, 0, `problems_sanitation_facicility/other`),
       `diff_or_shocks/other` = ifelse(is.na(other_shock_diff) & `diff_or_shocks/other` == 1, 0, `diff_or_shocks/other`),
       `hh_situation/other` = ifelse(is.na(other_sit) & `hh_situation/other` == 1, 0, `hh_situation/other`),
#       `main_source_electricity/other` = ifelse(is.na(other_source_electricity) & `main_source_electricity/other` == 1, 0, `main_source_electricity/other`),
#       `preferred_language_spoken_info/other` = ifelse(is.na(other_spoken_info) & `preferred_language_spoken_info/other` == 1, 0, `preferred_language_spoken_info/other`),
       `support_regular_learning/other` = ifelse(is.na(other_support_home) & `support_regular_learning/other` == 1, 0, `support_regular_learning/other`),
       `vunerability_type_hh/other` = ifelse(is.na(other_vun_type) & `vunerability_type_hh/other` == 1, 0, `vunerability_type_hh/other`),
       `main_source_water/other` = ifelse(is.na(other_water) & `main_source_water/other` == 1, 0, `main_source_water/other`),
       `problems_water_access/other` = ifelse(is.na(other_water_access_problem) & `problems_water_access/other` == 1, 0, `problems_water_access/other`),
#       `written_info_language/other` = ifelse(is.na(other_written_info) & `written_info_language/other` == 1, 0, `written_info_language/other`))
)

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
data_kobo_updated["main_source_water"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "main_source_water/")
data_kobo_updated["problems_water_access"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "problems_water_access/")
data_kobo_updated["problems_sanitation_facicility"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "problems_sanitation_facicility/")
data_kobo_updated["adaptation_to_sanitation"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "adaptation_to_sanitation/")
data_kobo_updated["access_law_enforement_authorities"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "access_law_enforement_authorities/")
data_kobo_updated["barriers_boys_faced"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "barriers_boys_faced/")
data_kobo_updated["barriers_girls_faced"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "barriers_girls_faced/")
data_kobo_updated["support_regular_learning"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "support_regular_learning/")
data_kobo_updated["hh_assitance_recieved"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "hh_assitance_recieved/")
data_kobo_updated["prefer_info_assistance"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "prefer_info_assistance/")

# Check before/after ------------------------------------------------------

id_to_check <- "7596d734-226d-415a-8393-1f8e60da069b"

pre <- data_kobo %>% 
  filter(uuid == id_to_check) %>% 
  select(contains("secondary_income"), other_income_secondary)

post <- data_kobo_updated %>% 
  filter(uuid == id_to_check) %>% 
  select(contains("secondary_income"), other_income_secondary)

write_csv(data_kobo_updated, "final_output/temporary_files/data_kobo_updated_new_otherchecks_temp.csv")
