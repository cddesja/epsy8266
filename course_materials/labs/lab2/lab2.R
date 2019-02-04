# load lavaan
library(lavaan)


#
## Model 1 ----
#

set.seed(123512)
#
#
# Setting up conditions for model 1
#
#

# relationship between X and W
a <- .4

# relationship between W and Y
b <- .3

# relationship between X and Y
c <- .5

# how many observations should we generate.
# the more we generate the closer we will get to the true parameters
# (i.e, the small error we'll have) recall sampling distributions.
n <- 500

# let's generate X to be a random normal variable
X <- rnorm(n = n, mean = 0, sd = 1)

# let's generate W
W <- a*X + rnorm(n, mean = 0, sd = sqrt(1 - a^2))
# - The .4 is the standardized regression weight between X and W
# - The rnorm() stuff adds the residual variance to make the correlation between
# - X and W .4
# - You can verify this by setting n to a really large number and doing
# - cor(X, W)

# Now let's generate Y
Y <- b*W + c*X + rnorm(n, mean = 0, sd = sqrt(1 - (b^2 + c^2 + 2*b*c*a)))
dat <- data.frame(X, W, Y)

# And let's fit a path model and omit W
mod <- '
Y ~ X
'
fit <- sem(model = mod, data = dat)
summary(fit)
params <- parameterEstimates(fit)
params[params$lhs == "Y" & params$rhs == "X", "est"]

set.seed(125312)

# how many replicates should we use?
nsim <- 2000

# run the simulation
# notice that everything in the expr argument was defined earlier.
runSim <- replicate(nsim, expr = {

  # all these lines are the same as above
  X <- rnorm(n = n, mean = 0, sd = 1)
  W <- a*X + rnorm(n, mean = 0, sd = sqrt(1 - a^2))
  Y <- b*W + c*X + rnorm(n, mean = 0, sd = sqrt(1 - (b^2 + c^2 + 2*b*c*a)))
  dat <- data.frame(X, W, Y)
  fit <- sem(model = mod, data = dat)
  params <- parameterEstimates(fit)
  params[params$lhs == "Y" & params$rhs == "X", "est"]
  })

# let's plot the results
hist(runSim)

# Now calculate the mean
mean(runSim)

#
## Model 2 ----
#

set.seed(125312)
#
#
# Setting up conditions for model 2
#
#

#
# Change one of these parameters
#
# relationship between X and w
a <- .4

# relationship between W and Y
b <- .3

# relationship between X and Y
c <- .5
#
#
#

# how many observations should we generate.
# the more we generate the closer we will get to the true parameters
# (i.e, the small error we'll have) recall sampling distributions.
n <- 500
# how many replicates should we use?
nsim <- 2000

# run the simulation
# notice that everything in the expr argument was defined earlier.
runSim <- replicate(nsim, expr = {

  # all these lines are the same as above
  X <- rnorm(n = n, mean = 0, sd = 1)
  W <- a*X + rnorm(n, mean = 0, sd = sqrt(1 - a^2))
  Y <- b*W + c*X + rnorm(n, mean = 0, sd = sqrt(1 - (b^2 + c^2 + 2*b*c*a)))
  dat <- data.frame(X, W, Y)
  fit <- sem(model = mod, data = dat)
  params <- parameterEstimates(fit)
  params[params$lhs == "Y" & params$rhs == "X", "est"]
  })

# let's plot the results
hist(runSim)

# Now calculate the mean
mean(runSim)

#
## Model 3 ----
#

set.seed(125312)
#
#
# Setting up conditions for model 3
#
#

#
# Change one or more of these (if necessary)
#
# relationship between W and X
a <- .4

# relationship between W and Y
b <- .3

# relationship between X and Y
c <- .5
#
#
#

# how many observations should we generate.
# the more we generate the closer we will get to the true parameters
# (i.e, the small error we'll have) recall sampling distributions.
n <- 500

# how many replicates should we use?
nsim <- 2000

# run the simulation
runSim <- replicate(nsim, expr = {

  # all these lines are the same as above
  W <- rnorm(n = n, mean = 0, sd = 1)
  X <- a*W + rnorm(n, mean = 0, sd = sqrt(1 - a^2))
  Y <- b*W + c*X + rnorm(n, mean = 0, sd = sqrt(1 - (b^2 + c^2 + 2*b*c*a)))
  dat <- data.frame(X, W, Y)
  fit <- sem(model = mod, data = dat)
  params <- parameterEstimates(fit)
  params[params$lhs == "Y" & params$rhs == "X", "est"]
  })

# let's plot the results
hist(runSim)

# Now calculate the mean
mean(runSim)

#
## Model 4 ----
#

set.seed(125312)
#
#
# Setting up conditions for model 4
#
#

#
# Change one or more of these  (if necessary)
#
# relationship between W and X
a <- .4

# relationship between W and Y
b <- .3

# relationship between X and Y
c <- .5
#
#
#

# how many observations should we generate.
# the more we generate the closer we will get to the true parameters
# (i.e, the small error we'll have) recall sampling distributions.
n <- 500

# how many replicates should we use?
nsim <- 2000

# run the simulation
# notice that everything in the expr argument was defined earlier.
runSim <- replicate(nsim, expr = {

  # all these lines are the same as above
  W <- rnorm(n = n, mean = 0, sd = 1)
  X <- a*W + rnorm(n, mean = 0, sd = sqrt(1 - a^2))
  Y <- b*W + c*X + rnorm(n, mean = 0, sd = sqrt(1 - (b^2 + c^2 + 2*b*c*a)))
  dat <- data.frame(X, W, Y)
  fit <- sem(model = mod, data = dat)
  params <- parameterEstimates(fit)
  params[params$lhs == "Y" & params$rhs == "X", "est"]
  })

# let's plot the results
hist(runSim)

# Now calculate the mean
mean(runSim)

#
## Model 5 ----
#

set.seed(125312)
#
#
# Setting up conditions for model 5
#
#

# relationship between X and W
a <- .4

# relationship between W and Y
b <- .3

# relationship between X and Y
c <- .5
#
#
#

# how many observations should we generate.
# the more we generate the closer we will get to the true parameters
# (i.e, the small error we'll have) recall sampling distributions.
n <- 500

set.seed(125312)

# how many replicates should we use?
nsim <- 2000

# create the misspecified model - this one includes W
mod.w <- '
  Y ~ X + W
'

# run the simulation
# notice that everything in the expr argument was defined earlier.
runSim <- replicate(nsim, expr = {

  # all these lines are the same as above
  X <- rnorm(n = n, mean = 0, sd = 1)
  Y <- c*X + rnorm(n, mean = 0, sd = sqrt(1 - c^2))
  W <- b*Y + a*X + rnorm(n, mean = 0, sd = sqrt(1 - (b^2 + a^2 + 2*b*c*a)))
  dat <- data.frame(X, W, Y)
  fit <- sem(model = mod, data = dat)
  fit.w <- sem(model = mod.w, data = dat)
  params <- parameterEstimates(fit)
  params.w <- parameterEstimates(fit.w)
  c(params[params$lhs == "Y" & params$rhs == "X", "est"],
    params.w[params.w$lhs == "Y" & params.w$rhs == "X", "est"])
  })

# Now print the means
rownames(runSim) <- c("Correct", "Misspecified")
rowMeans(runSim)

