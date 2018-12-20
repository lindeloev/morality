###############################
# HANDY INFERENTIAL FUNCTIONS #
###############################

# PLOTTING
# To give confidence intervals on binomial rates
binom.summary = function(x) {
  result = binom.test(sum(x), length(x))
  return(data.frame(y=result$estimate, ymin=result$conf.int[1], ymax=result$conf.int[2]))
}

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}


# FITTING
# Likelihood ratio test for LMM
LRT = function(D, full, null) {
  fit.full <<- lmer(full, D, REML=FALSE)
  fit.null <<- lmer(null, D, REML=FALSE)
  
  LRT_report()
}

# Likelihood ratio test for binomial mixed model
LRT_binom = function(D, full, null) {
  fit.full <<- glmer(full, D, family=binomial)
  fit.null <<- glmer(null, D, family=binomial)
  LRT_report(OR=TRUE)
}


# Bayes Factors using BayesFactor::kmBF
BF = function(D, full.fixed, full.random, null.fixed, null.random) {
  fit.b.full <<- lmBF(full.fixed, D, whichRandom=full.random)
  fit.b.null <<-lmBF(null.fixed, D, whichRandom=null.random)
  return(fit.full/fit.null)
}


# PRINTING
printBF = function(BF) ifelse(BF < 1, paste(round(1/BF, 1), '*', sep=''), round(BF, 1))

# Show the results of a Likelihood-Ratio test
# OR = Odds Ratio
LRT_report = function(OR=FALSE, lmer=TRUE) {
  library(car)
  # Fit stuff
  result <<- anova(fit.full, fit.null)
  print(result)
  fit <<- summary(fit.full)
  
  # Coefficients and bayes factors
  print(fit$coefficients)
  if(lmer) {
    intervals = cbind(fixef(fit.full), confint(fit.full, parm='beta_', method='Wald'))
    if(OR) print(exp(intervals))  # Odds ratio
    else print(intervals)  # parameter estiamtes
  }
  BF = exp((result$BIC[1] - result$BIC[2])/2)
  print(paste('BIC-based Bayes Factor:', printBF(BF)))
  # 
  # # Assess model assumptions
  # par(mfrow=c(1, 2))
  # qqPlot(fit$residuals)  # qq plot
  # hist(fit$residuals, freq=F, breaks=50)
  # curve(dnorm(x, 0, sd(fit$residuals)), add=T)
}
