# Equivalence, reliability, and ordinal indicators 

# read in data and load the lavaan package ----
library(lavaan) 
hs.data <- read.csv("https://github.com/cddesja/epsy8266/raw/master/course_materials/data/HolzingerSwineford1939.csv")

# Below is the basic syntax for a CFA using lavaan. Note, we are using lavaan.
# First, thing we need to add identify this model by adding a scale.
# How can we do this below?

# congeneric ----
mod.con <- '
  # define the latent variable, verbal
  verbal =~ 1*general + paragrap + sentence + wordc + wordm
  
  # factor variance, phi
  verbal ~~ verbal
  
  # error terms 
  general ~~ general
  paragrap ~~ paragrap
  sentence ~~ sentence
  wordc ~~ wordc
  wordm ~~ wordm
'
fit.con <- lavaan(mod.con, hs.data)
summary(fit.con)



# tau-equivalent ----
# What do we need to do now?
mod.tau <- '
  # define the latent variable, verbal
  verbal =~ 1*general + 1*paragrap + 1*sentence + 1*wordc + 1*wordm
  
  # factor variance, phi
  verbal ~~ verbal
  
  # error terms 
  general ~~ general
  paragrap ~~ paragrap
  sentence ~~ sentence
  wordc ~~ wordc
  wordm ~~ wordm
'
fit.tau <- lavaan(mod.tau, hs.data)
summary(fit.tau)
anova(fit.tau, fit.con)



# parallel-equivalent ----
# What do we need to do now?
mod.par <- '
  # define the latent variable, verbal
  verbal =~ 1*general + 1*paragrap + 1*sentence + 1*wordc + 1*wordm
  
  # factor variance, phi
  verbal ~~ verbal
  
  # error terms 
  general ~~ e1*general
  paragrap ~~ e1*paragrap
  sentence ~~ e1*sentence
  wordc ~~ e1*wordc
  wordm ~~ e1*wordm
'
fit.par <- lavaan(mod.par, hs.data)
anova(fit.par, fit.tau)



# composite reliability ----
# how do we calculate it from the parameter estimates output?
params.fit <- parameterEstimates(fit.con)

























#' Calculate composite reliability -----
#' 
#' Assumes simple structure
#' @param x a fitted lavaan model
#' 
comp.reliab <- function(x){
  # extract the parameter estimates
  ests <- data.frame(lhs = x@ParTable$lhs, 
                     op = x@ParTable$op,
                     rhs = x@ParTable$rhs,
                     est = x@ParTable$est,
                     stringsAsFactors = FALSE)
  # look for multiple factors
  lf <- unique(ests[grep("=~", ests$op), "lhs"])
  
  # initiate a variable to save reliabilities
  rel <- rep(NA, length(lf))
  names(rel) <- lf
  
  # calculate the reliabilities
  for(i in 1:length(lf)){
    man.vars <- subset(ests, lhs == lf[i] & op == "=~", select = rhs, drop = TRUE)
    sum.pc <- sum(subset(ests, lhs == lf[i] & rhs %in% man.vars, select = est))^2
    fact.var <- subset(ests, lhs == lf[i] & rhs == lf[i], select = est, drop = TRUE)
    error.var <- sum(subset(ests, lhs %in% man.vars & rhs %in% man.vars, select = est))
    rel[i] <- (sum.pc * fact.var) / (sum.pc * fact.var + error.var)
  }
  
  # print the reliability
  rel
}
comp.reliab(fit.con)  

# Alternatively (more easily) via semTools
semTools::reliability(fit.con) # we want omega 
# note, you also get average variance extracted (avevar) printed. 



# ordinal confirmatory factor analysis ----
lsat6 <- data.frame(psych::lsat6)
lsat.mod <- "
  lsat =~ Q1 + Q2 + Q3 + Q4 + Q5
"
lsat.fit <- cfa(lsat.mod, lsat6, ordered = paste0("Q", 1:5), parameterization = "<INSERT>")
summary(lsat.fit)




