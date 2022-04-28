#subject is subject column
#between_sub_vars is list of between subject vars. Example -- c("explicit", "probability")
#within_sub_vars is list of within subject vars. Example -- c("word_associat_size", "size_rew_font", "size_rew_shape", "StroopCond", "shaperewval", "block")
#Accuracy_col is a column with whether trial is accurate or not. Accurate is 1 and incorrect is 0. Example -- 'correct'
#DV_col is a column with DV. Example -- 'rt'

Within_Correct  <- function(dataset, ID, between_sub_vars, within_sub_vars,  DV_col) {

  vars_2_group_between_ID <- paste(ID, toString(between_sub_vars) )
  vars_2_group_between <- toString(between_sub_vars)
  
  
  dataset$dv <- dataset[[DV_col]]   
  
  df <- dataset %>%
  group_by({vars_2_group_between}) %>%
  
  mutate(grndmeandv = mean(dv, na.rm = T)) %>% 
  
  group_by({vars_2_group_between_ID}) %>%
  
  #average over conditions and get average per subj
  mutate(condmean_perpardv = mean(dv, na.rm = T)) %>%
  
  #make adjustment factor to add to all scores
  mutate(adj_factdv = grndmeandv - condmean_perpardv) %>%
  
  ungroup()%>%
  #adjusted scores
  mutate(dv_adj = dv + adj_factdv)
  return(df)
}