#subject is subject column
#between_sub_vars is list of between subject vars. Have in the dots
#DV_col is a column with DV. Example -- 'rt'

Within_Correct  <- function(dataset, subject, DV_col, ...) {

 subject <- enquo(subject)
DV_col <- enquo(DV_col)


  df <- dataset %>%
    #group by between subject vars
  group_by(...) %>%
  mutate(grndmeand = mean(dv, na.rm = T)) %>%
    #group by ID
  group_by(!!subject) %>%

  #average over conditions and get average per subj
  mutate(condmean_perpar = mean(dv, na.rm = T)) %>%

  #make adjustment factor to add to all scores
  mutate(adj_fact = grndmeand - condmean_perpar) %>%

  ungroup()%>%
  #adjusted scores
  mutate(dv_adj = dv + adj_fact)
  return(df)
}
