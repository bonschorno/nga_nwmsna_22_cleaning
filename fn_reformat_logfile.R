reformat_logfile <- function(df, variable_name, variable_to_zero){
  
  formatted_df <- df %>% 
    filter(question.name == variable_name) %>% 
    select(uuid, new.value) %>% 
    dummy_cols(select_columns = 'new.value',
               split = ",",
               ignore_na = TRUE,
               remove_selected_columns = TRUE) %>% 
    mutate(!!variable_to_zero := 0) %>% 
    pivot_longer(-uuid, names_to = "question.name", values_to = "new.value") %>% 
    filter(new.value == 1 | question.name == variable_to_zero) %>% 
    mutate(question.name = gsub(pattern = "new.value_", replacement = "", x = question.name)) %>% 
    mutate(old.value = ifelse(question.name == variable_to_zero, 1, 0)) %>% 
    mutate(new.value = as.character(new.value))
  
  return(formatted_df)
  
}