rewrite_other <- function(df, dummy_string_pattern){
  
  target_var <- gsub(pattern = "/", replacement = "", x = dummy_string_pattern)
  
  names_columns <- df %>%
    select(contains(dummy_string_pattern)) %>%
    names()
  
  changed_df <- df %>% 
    mutate(!!target_var := apply(df[names_columns], 1, function(x) paste(names_columns[x == 1], collapse = " "))) %>% 
    mutate(!!target_var := str_remove_all(.data[[target_var]], pattern = dummy_string_pattern),
           !!target_var := ifelse(str_detect(string = .data[[target_var]], pattern = "NA"), NA, .data[[target_var]])) %>% 
    select({{target_var}})
  
  return(changed_df)
  
  
}