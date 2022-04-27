#Outlier_Trim_Function

#subject is subject column
#between_sub_vars is list of between subject vars. Example -- c("explicit", "probability")
#within_sub_vars is list of within subject vars. Example -- c("word_associat_size", "size_rew_font", "size_rew_shape", "StroopCond", "shaperewval", "block")
#Accuracy_col is a column with whether trial is accurate or not. Accurate is 1 and incorrect is 0. Example -- 'correct'
#RT_col is a column with RTs. Example -- 'rt'
#SD_cutoff is the number of SDs for outlier trim.

outliertrim  <- function(dataset, ID, between_sub_vars, within_sub_vars, Accuracy_col, RT_col, SD_cutoff) {

vars_2_group <- paste(ID, toString(between_sub_vars), toString(within_sub_vars), sep = ", ")
vars_2_group_between_ID <- paste(ID, toString(between_sub_vars) )
vars_2_group_between <- toString(between_sub_vars)


dataset$rt <- dataset[[RT_col]] 
dataset$correct <- dataset[[Accuracy_col]]
  
  df <- dataset %>% 
  group_by({vars_2_group}) %>%
  mutate(new_DV_col = (sum(correct, na.rm = T) / length(rt)*100)) %>%
  
  filter(correct == 1) %>% #use only correct trials for analysis
  mutate(starttrials = length(rt)) %>% #get number of trials before trim
  mutate(avg = mean(rt), stdev = sd(rt)) %>% #get initial average
  filter(rt <= (SD_cutoff*stdev)+avg) %>%  
  filter(rt >= avg - (SD_cutoff*stdev)) %>% 
  mutate(avg = mean(rt), stdev = sd(rt)) %>%  #get second average
  filter(rt <= (SD_cutoff*stdev)+avg) %>% 
  filter(rt >= avg - (SD_cutoff*stdev)) %>%
      
  mutate(endtrials = length(rt)) %>%
  mutate(percaccept = ((endtrials-starttrials)/starttrials)*100) %>% #percent excluded with trim
  
  #get grand mean for each between subjects group
  ungroup() %>%
  group_by({vars_2_group_between}) %>%
  
  mutate(grndmeanRT = mean(rt, na.rm = T)) %>% 
  
  group_by({vars_2_group_between_ID}) %>%
  
  #average over conditions and get average per subj
  mutate(condmean_perparRT = mean(rt, na.rm = T)) %>%
  
  #make adjustment factor to add to all scores
  mutate(adj_factRT = grndmeanRT - condmean_perparRT) %>%
  
  ungroup()%>%
  #adjusted scores
  mutate(rt_adj = rt + adj_factRT) 
  return(df)
}