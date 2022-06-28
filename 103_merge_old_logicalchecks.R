# Merge old logical checks

rm(list = ls())

library(tidyverse)
library(readxl)
source("scripts/fn_rewrite_other_column.R")

old_logicalchecks1 <- read_xlsx("old_logicalchecks/Data_logical_checks_R.xlsx") %>% 
  select(uuid, question.name, new.value) 

old_logicalchecks2 <- read_xlsx("old_logicalchecks/Data_logical_checks_R_2.xlsx") %>% 
  select(uuid, question.name, new.value) 

old_logicalchecks <- bind_rows(old_logicalchecks1, old_logicalchecks2)

rm(old_logicalchecks1, old_logicalchecks2)

# question names
unique(old_logicalchecks$question.name)

# check for duplicates (four duplicates for some reason)
old_logicalchecks %>% 
  group_by(uuid, question.name) %>% 
  count() %>% 
  filter(n > 1) %>% 
  view()

# extract question names
question_names <- old_logicalchecks %>% 
  distinct(question.name) %>% 
  pull()

vars_to_ignore <- c("uuid", "lack_income_cope", 
                    "shelter_enclosure_issues", "nearest_health_care")

# prepare data
data_list <- list()

# Note: The loop throws out a warning which can be ignored since it's caused by the last line that
# changes columns from string to numeric variables

for (variable in question_names) {
  data_list[[variable]] <- old_logicalchecks %>% 
    filter(question.name == {{variable}}) %>% 
    distinct(uuid, .keep_all = TRUE) %>% # removing the duplicates from above
    ungroup() %>% 
    select(uuid, question.name, new.value) %>% 
    pivot_wider(uuid, names_from = "question.name", values_from = "new.value") %>% 
    mutate(across(-one_of(vars_to_ignore), as.numeric))
}

# Clean data --------------------------------------------------------------------

# read in raw data
data_kobo <- read_csv("final_output/temporary_files/data_kobo_updated_new_otherchecks_temp.csv") 

# initialise updated kobo data
data_kobo_updated <- data_kobo

# loop over list elements to changes values in raw dataset and implement them in updated dataset
for (variable in question_names) {
  data_kobo_updated <- data_kobo_updated %>% 
    rows_upsert(data_list[[variable]])
}

data_kobo_updated["lack_income_cope"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "lack_income_cope/")
data_kobo_updated["shelter_enclosure_issues"] <- rewrite_other(data_kobo_updated, dummy_string_pattern = "shelter_enclosure_issues/")

# Check before/after ------------------------------------------------------

# for shelter_enclosure_issues
sample_id <- "d54868d1-0585-4bc2-9831-695e37b0d732"

pre <- data_kobo %>% 
  filter(uuid == sample_id) %>% 
  select(contains("shelter_enclosure_issues"))

post <- data_kobo_updated %>% 
  filter(uuid == sample_id) %>% 
  select(contains("shelter_enclosure_issues"))

# for lack_income_cope
sample_id <-"0b682df2-f93a-4286-9839-d4ca2891c49d"

pre <- data_kobo %>% 
  filter(uuid == sample_id) %>% 
  select(contains("lack_income_cope"))

post <- data_kobo_updated %>% 
  filter(uuid == sample_id) %>% 
  select(contains("lack_income_cope"))

write_csv(data_kobo_updated, "final_output/temporary_files/data_kobo_updated_old_logicalchecks_temp.csv") 
