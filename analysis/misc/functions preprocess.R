###########################
# PREPROCESSING FUNCTIONS #
###########################
source('misc/functions utility.R')

fix_questions = function(D) {
  # Move questions to columns and make numeric data... numeric!
  question_labels = c('qStress', 'qSatisfied', 'qFrustration', 'qDifficulty', 'qEffort', 'qAttitude', 'qBirthweight', 'qFriends')
  for(question in question_labels) {
    tmp = subset(D, condition == question)  # rows with this question for each subject
    D[, question] = mapvalues2(D$id, tmp$id, tmp$recallAns)  # map from to column
    D[, paste(question, 'RT', sep='')] = mapvalues2(D$id, tmp$id, tmp$recallRT)
  }
  D = ize(D, question_labels, make_numeric)  # All questions are numerically responded to
  D = droplevels(subset(D, !condition %in% question_labels))  # Remove these rows
  D = droplevels(subset(D, !condition %in% c('qCheck1', 'qCheck2', 'qCheck3', 'qCheck4', 'qFeedback', 'qAbout', '')))
  
  # Create an index of subjectively experienced depletion
  D$subjectiveDepletion = D$qDifficulty + D$qStress + D$qEffort + D$qFrustration
  
  return(D)
}

fix_matrix_cols = function(D) {
  # OBS: MAKES A DIFFERENCE WHETHER FIRST LINE IS COMMENTED OR NOT! BUG?
  # Correct RT being off for some subjects and make RT vectors of equal length (=7), right-padded with NAs where equation was shown
  D$equationRTs = apply(D, 1, function(x) as.numeric(unlist(strsplit(as.character(x['equationRTs']), ', '))))
  D = plyr::ddply(D, plyr::.(id), function(df) {
    df$equationRTs = t(sapply(df$equationRTs, '[', 1:7))
    test = any(df$equationRTs < 0)
    test = ifelse(is.na(test), FALSE, test)  # convert NA to FALSE (do not correct)
    if(test) df$equationRTs = df$equationRTs - runif(1, max(df$equationRTs, na.rm=T) - 5, min(df$equationRTs, na.rm=T) - 1.5)  # subtract a random value to get RT between 1.5 and 5
    df
  })
  
  # Convert comma-separated strings to lists
  D$equationAnss = t(sapply(apply(D, 1, function(x) as.numeric(unlist(strsplit(as.character(x['equationAnss']), ', ')))), '[', 1:7))
  D$equationCorrect = t(sapply(apply(D, 1, function(x) as.numeric(unlist(strsplit(as.character(x['equationCorrect']), ', ')))), '[', 1:7))
  D$equationScores = t(sapply(apply(D, 1, function(x) as.numeric(unlist(strsplit(as.character(x['equationScores']), ', ')))), '[', 1:7))
  D$recallAns = t(sapply(apply(D, 1, function(x) unlist(strsplit(as.character(x['recallAns']), ', '))), '[', 1:7))
  D$encode = t(sapply(apply(D, 1, function(x) unlist(strsplit(as.character(x['encode']), ', '))), '[', 1:7))
  
  # Handy summaries of the above
  D$equationRT = rowMeans(D$equationRTs, na.rm=T)  # Mean equation RT per trial
  D$equationCorrectness = D$equationScore / D$span  # Mean equation correctness per trial (accuracy)
  
  return(D)
}

fix_other = function(D) {
  # Change data format of some columns
  D$stimType = factor(D$stimType, labels=c('Faces', 'Letters'))
  D$level = factor(D$level, levels=c('1-3', '3-5', '5-7', '1-7'))
  
  return(D)
}



# # SETTINGS
# PGG_STREAK_TRIALS = 4  # how many identical sequential PGG ratings to count as invalid (set as NA)
# PGG_STREAK_SUBJECT = 15  # how many identical PGG ratings to mark subject as bad
# 
# EQ_ACC = 0.8  # minimum average equation accuracy before excluding subject
# EQ_FAST = 1.0  # equation answers below this time (s) are marked as NA
# EQ_SLOW = 2.5  # equation answers above this time (SD upper threshold) are marked as NA
# 
# TRIALS_MINIMUM = 10  # Minimum number of trials after pruning to count as good
# 
# 
# # Function to mark individual trials on various criteria (adds "df$quality.XXX" columns
# check_quality = function(D) {
#   # Reaction times on equations
#   RTs = D$equationRTs  # short
#   D$quality.eq_fast = rowSums(!(RTs > EQ_FAST), na.rm=T) == 0  # Mark fast responses
#   D$quality.eq_slow = rowSums(!(log(RTs) < median(RTs, na.rm=T) + EQ_SLOW * sd(log(RTs), na.rm=T)), na.rm=T) == 0  # Remove slow responses
#   D$quality.eq_timeout = rowSums(apply(RTs, 2, is.na)) == 7 - D$span  # timeouts: number of NA's should equal 7 minus [span]
#   D$quality.eq_acc = mean(D$equationCorrectness, na.rm=T) > EQ_ACC  # Acceptable mean correctness for participant
#   
#   # Everything needs to be OK
#   D$quality.cs =
#     D$quality.eq_fast &
#     D$quality.eq_slow &
#     D$quality.eq_timeout & 
#     D$quality.eq_acc
#   
#   
#   # Add criterion that a minimum number of "good" trials are needed
#   D$quality.trials = sum(D$quality.cs) >= TRIALS_MINIMUM
#   D$quality.cs = D$quality.cs & D$quality.trials
#   
#   return(D)
# }
# 
# ddply_quality = function(D) {
#   # Manual ddply since ddply removes columns 2+ of matrix columns.
#   # Do it on id-condition subsets
#   D.tmp = data.frame()  # a new one since it's going to contain more columns
#   for(condition_this in levels(D$condition)) {
#     D.cond = droplevels(subset(D, condition == condition_this))
#     for(id_this in levels(D.cond$id)) {
#       # Add quality info but do not include if there's too little data to even compute anything
#       D.cond.id = droplevels(subset(D.cond, id==id_this))  # Subset by ID
#       if(nrow(D.cond.id) != 1) {
#         D.tmp = rbind(D.tmp, check_quality(D.cond.id))
#       }
#     }
#   }
#   return(D.tmp)
# }