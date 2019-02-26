model <- '
  y1 ~ a*x1
  y5 ~ c*x1 + b*y1

  # indirect effects
  ab := a*b
  totl := c + a*b
'
fit <- sem(model, data = PoliticalDemocracy)
summary(fit)

# define the function to extract the indirect effect
boot.fun <- function(x) {
  parameterEstimates(x)[parameterEstimates(x)$label %in% c("ab"), "est"]
}

# run the bootstrap
# - by default this is nonparameteric
ab.dist <- bootstrapLavaan(fit, R = 5000, FUN = boot.fun)
ab.dist <- data.frame(ab.dist)
names(ab.dist) <- c("ab")
hist(ab.dist$ab)
quantile(ab.dist$ab, probs = c(.025, .975))

