library(tidyverse)
source('misc/functions preprocess.R')

################################
# PREPROCESS DATA FROM EXP 1+2 #
################################
D1 = read.csv('data/pgg_all.csv', sep='\t') %>%  # data file containing all trials by all subjects
  mutate(
    exp = ifelse(level == '1-7', 1, 2)
  ) %>%
  
  # Restructure and add info
  fix_questions() %>%
  fix_matrix_cols() %>%
  fix_other()

# Needs to be done old-style because tidyverse doesn't like the matrix columns
# Time: per-trial duration and elapsed time within subject
D1$trialDuration = D1$span * ifelse(D1$stimType == 'Faces', 2, 0.5) + rowSums(D1$equationRTs, na.rm=T) + D1$recallRT + D1$pggRT
D1$time_secs = ave(D1$trialDuration, D1$id, FUN=function(x) cumsum(coalesce(x, 0)) + x*0)  # cumsum with NAs; from https://stackoverflow.com/a/25576972/1297830
start_times = aggregate(time_secs ~ id, subset(D1, condition=='experiment'), FUN=min)  # per-subject start times
D1$time_secs = D1$time_secs - mapvalues2(D1$id, from=start_times$id, to=start_times$time_secs)  # Make it start at zero
D1$time_hours = (D1$time_secs - mean(D1$time_secs, na.rm=T)) / 3600  # scaled, for regression (interpret slope as per-hour

# Save as data.frame to preserve matrix columns
saveRDS(D1, 'data/pgg.Rda')
