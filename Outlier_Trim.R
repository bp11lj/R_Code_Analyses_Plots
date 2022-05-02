#Outlier_Trim_Function

#ENSURE ONLY CORRECT TRIALS

#subject is subject column
#DV_col is a column with DV. Example -- 'rt'
#SD_cutoff is the number of SDs for outlier trim.

outliertrim  <- function(dataset, ID, DV_col, SD_cutoff) {





dataset$dv <- dataset[[DV_col]]

  df <- dataset %>%
  mutate(starttrials = length(dv)) %>% #get number of trials before trim
  mutate(avg = mean(dv), stdev = sd(dv)) %>% #get initial average
  filter(dv <= (SD_cutoff*stdev)+avg) %>%
  filter(dv >= avg - (SD_cutoff*stdev)) %>%
  mutate(avg = mean(dv), stdev = sd(dv)) %>%  #get second average
  filter(dv <= (SD_cutoff*stdev)+avg) %>%
  filter(dv >= avg - (SD_cutoff*stdev)) %>%

  mutate(endtrials = length(dv))
 # mutate(percaccept = ((endtrials-starttrials)/starttrials)*100) %>% #percent excluded with trim


  #print(mean(df$percaccept))
  return(df)
}
