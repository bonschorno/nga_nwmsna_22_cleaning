create_separate_df <- function(df, question_name, column_to_zero){
  
  selection_df <- df %>% 
    filter(question.name == question_name) %>% 
    select(uuid, new.value)
  
  output_df <- dummy_cols(selection_df, 
                          select_columns = 'new.value',
                          split = ",",
                          ignore_na = TRUE,
                          remove_selected_columns = TRUE) %>% 
    mutate(!!column_to_zero := 0) %>% 
    pivot_longer(-uuid, names_to = "question.name", values_to = "new.value") %>% 
    mutate(question.name = gsub(pattern = "new.value_", replacement = "", x = question.name)) %>% 
    mutate(new.value = as.character(new.value))
  
  return(output_df)
  
}