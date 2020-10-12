library(tidyverse)
source('misc/functions preprocess.R')

###############################
# LOAD DATA FROM EXP 2 (DOTS) #
###############################

# This is in a much nicer format given the experience from exp 1+2, so less processing needed.
# See how Dots-data (D2) was merged in the end of this script.
D2 = read.table('data/dots_all.csv', sep='\t', header=T) %>%  # "D" for "DATA"
  mutate(
    dotsReceiver = factor(factor(dotsReceiver, labels=c('Other', 'Self')), levels=c('Self', 'Other')),  # New labels and label order
    exp_part = ifelse(level == '1-7', "dots_concurrent", "dots_cumulative")
  ) %>%
  
  # Call preprocessing functions
  fix_questions() %>%
  fix_matrix_cols() %>%
  fix_other()

# From absolute UTC time to relative-to-start wall time
times = aggregate(utc_start ~ id, subset(D2, condition=='experiment'), FUN=min)  # per-subject start times
D2$time_secs = D2$utc_start - mapvalues2(D2$id, from=times$id, to=times$utc_start)  # Make it start at zero
D2$time_hours = scale(D2$time_secs, center=T, scale=3600)  # scaled, for regression (interpret slope as per-hour)
D2$time_hours = c(D2$time_hours)  # Vector, not matrix

# Save as data.frame to preserve matrix columns
saveRDS(D2, 'data/dots.Rda')
