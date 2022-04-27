#Outlier_Trim_Function

#ENSURE ONLY CORRECT TRIALS

#subject is subject column
#between_sub_vars is list of between subject vars. Example -- c("explicit", "probability")
#within_sub_vars is list of within subject vars. Example -- c("word_associat_size", "size_rew_font", "size_rew_shape", "StroopCond", "shaperewval", "block")
#Accuracy_col is a column with whether trial is accurate or not. Accurate is 1 and incorrect is 0. Example -- 'correct'
#DV_col is a column with DV. Example -- 'rt'
#SD_cutoff is the number of SDs for outlier trim.

outliertrim  <- function(dataset, ID, between_sub_vars, within_sub_vars,  DV_col, SD_cutoff) {

vars_2_group <- paste(ID, toString(between_sub_vars), toString(within_sub_vars), sep = ", ")
vars_2_group_between_ID <- paste(ID, toString(between_sub_vars) )
vars_2_group_between <- toString(between_sub_vars)


dataset$dv <- dataset[[DV_col]] 

  df <- dataset %>% 
  group_by({vars_2_group}) %>%

  mutate(starttrials = length(dv)) %>% #get number of trials before trim
  mutate(avg = mean(dv), stdev = sd(dv)) %>% #get initial average
  filter(dv <= (SD_cutoff*stdev)+avg) %>%  
  filter(dv >= avg - (SD_cutoff*stdev)) %>% 
  mutate(avg = mean(dv), stdev = sd(dv)) %>%  #get second average
  filter(dv <= (SD_cutoff*stdev)+avg) %>% 
  filter(dv >= avg - (SD_cutoff*stdev)) %>%
      
  mutate(endtrials = length(dv)) %>%
  mutate(percaccept = ((endtrials-starttrials)/starttrials)*100) %>% #percent excluded with trim
  
  #get grand mean for each between subjects group
  ungroup() %>%
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