# Equivalence, reliability, and ordinal indicators 

# read in data and load the lavaan package ----
library(lavaan) 
hs_data <- read.csv("https://github.com/cddesja/epsy8266/raw/master/course_materials/data/HolzingerSwineford1939.csv")

# Below is the basic syntax for a CFA using lavaan. Note, we are using lavaan.
# First, thing we need to add identify this model by adding a scale.
# How can we do this below?

# congeneric ----
mod_con <- '
  # define the latent variable, verbal
  verbal =~ general + paragrap + sentence + wordc + wordm
  
  # factor variance, phi
  verbal ~~ verbal
  
  # error terms 
  general ~~ general
  paragrap ~~ paragrap
  sentence ~~ sentence
  wordc ~~ wordc
  wordm ~~ wordm
'
fit_con <- lavaan(mod_con, hs_data)
summary(fit_con)



# tau-equivalent ----
# What do we need to do now?
mod_tau <- '
  # define the latent variable, verbal
  verbal =~ general + paragrap + sentence + wordc + wordm
  
  # factor variance, phi
  verbal ~~ verbal
  
  # error terms 
  general ~~ general
  paragrap ~~ paragrap
  sentence ~~ sentence
  wordc ~~ wordc
  wordm ~~ wordm
'
fit_tau <- lavaan(mod, hs.data)
summary(fit_tau)
anova(fit_tau, fit_con)



# parallel-equivalent ----
# What do we need to do now?
mod_par <- '
  # define the latent variable, verbal
  verbal =~ general + paragrap + sentence + wordc + wordm
  
  # factor variance, phi
  verbal ~~ verbal
  
  # error terms 
  general ~~ general
  paragrap ~~ paragrap
  sentence ~~ sentence
  wordc ~~ wordc
  wordm ~~ wordm
'
fit_par <- lavaan(mod_par, hs_data)
anova(fit_par, fit_tau)



# composite reliability ----
# how do we calculate it from the parameter estimates output?
params_fit <- parameterEstimates(fit_con)

























#' Calculate composite reliability -----
#' 
#' Assumes simple structure
#' @param x a fitted lavaan model
#' 
comp_reliab <- function(x){
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
    man_vars <- subset(ests, lhs == lf[i] & op == "=~", select = rhs, drop = TRUE)
    sum_pc <- sum(subset(ests, lhs == lf[i] & rhs %in% man_vars, select = est))^2
    fact_var <- subset(ests, lhs == lf[i] & rhs == lf[i], select = est, drop = TRUE)
    error_var <- sum(subset(ests, lhs %in% man_vars & rhs %in% man_vars, select = est))
    rel[i] <- (sum_pc * fact_var) / (sum_pc * fact_var + error_var)
  }
  
  # print the reliability
  rel
}
comp_reliab(fit_con)  

# Alternatively (more easily) via semTools
semTools::reliability(fit_con) # we want omega 
# note, you also get average variance extracted (avevar) printed. 



# ordinal confirmatory factor analysis ----
lsat6 <- data.frame(psych::lsat6)
lsat.mod <- "
  lsat =~ Q1 + Q2 + Q3 + Q4 + Q5
"
lsat.fit <- cfa(lsat.mod, lsat6, ordered = paste0("Q", 1:5), parameterization = "<INSERT>")
summary(lsat.fit)




