# Recreate Houghton & Jinkerson's (2007) model
# In this activity, I have given you the data, however, you need to convert it
# so that lavaan can use it.
# You then need to fit and estimate the model in lavaan
library(lavaan)
N <- 263
M <- c(3.96, 4.12, 4.13, 3.97, 3.61, 3.3, 2.13, 1.63, 1.99, 3.86, 3.62, 3.5)
SD <- c(.939, 1.017, .937, .562, .76, .524, .585, .609, .731, .711, 1.124, 1.001)
varNames <- c("jdiw1", "jdiw2", "jdiw3", "ford", "uf1", "uf2", "das1", "das2", "das3", "eba","st", "mi")
lowTri <- "
1
.668 1
.635 .599 1
.263 .261 .164 1
.290 .315 .247 .486 1
.207 .245 .231 .251 .449 1
-.206 -.182 -.195 -.309 -.266 -.142 1
-.280 -.241 -.238 -.344 -.305 -.230 .753 1
-.258 -.244 -.185 -.255 -.255 -.215 .554 .587 1
.08 .096 .094 -.017 .151 .141 -.074 -.111 .016 1
.061 .028 -.035 -.058 -.051 -.003 -.04 -.04 -.018 .284 1
.113 .174 .059 .063 .138 .044 -.119 -.073 -.084 .563 .379 1
"
cov.mat <- getCov(lowTri, names = varNames, sds = SD)

# step 1 ----
one.mod <- "
fac1 =~ eba + st + mi + das1 + das2 + das3 + jdiw1 + jdiw2 + jdiw3 + uf1 + uf2 + ford
"
one.fit <- cfa(mod = one.mod, sample.cov = cov.mat, sample.nobs = N)
fitmeasures(one.fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

# step 2 --- 
meas.mod <- '
cts =~ eba + st + mi
dtp =~ das1 + das2 + das3
js =~ jdiw1 + jdiw2 + jdiw3
swb =~ uf1 + uf2 + ford
'
meas.fit <- cfa(mod = meas.mod, sample.cov = cov.mat, sample.nobs = N)
anova(meas.fit, one.fit) # discriminant validity
summary(meas.fit, standardized = TRUE, fit.measures = TRUE)
lavResiduals(meas.fit, type = "cor")$cov
lavResiduals(meas.fit, type = "cor")$cov.z
modificationindices(meas.fit, sort. = TRUE)

# step 3 ----
sr.mod <- "
cts =~ eba + st + mi
dtp =~ das1 + das2 + das3
js =~ jdiw1 + jdiw2 + jdiw3
swb =~ uf1 + uf2 + ford
dtp ~ cts
swb ~ a*dtp
js ~ b*swb + c*dtp

# indirect effect
ab := a*b
"
sr.fit <- cfa(mod = sr.mod, sample.cov = cov.mat, sample.nobs = N)
anova(sr.fit, meas.fit)
summary(sr.fit, fit.measures = TRUE, standardized = TRUE)
lavResiduals(sr.fit, type = "cor")$cov
lavResiduals(sr.fit, type = "cor")$cov.z
modificationindices(sr.fit, sort. = TRUE)


meas.mod2 <- '
# define latent variables
cts =~ 1*eba + st + mi
dtp =~ 1*das1 + das2 + das3
js =~ 1*jdiw1 + jdiw2 + jdiw3
swb =~ 1*uf1 + uf2 + ford

# define lv cov/var
cts ~~ cts + dtp + js + swb
dtp ~~ dtp + js + swb
js ~~ js + swb
swb ~~ swb

# variance of manifest variables
eba ~~ eba
st ~~ st
mi ~~ mi
das1 ~~ das1
das2 ~~ das2
das3 ~~ das3
jdiw1 ~~ jdiw1
jdiw2 ~~ jdiw2
jdiw3 ~~ jdiw3
uf1 ~~ uf1
uf2 ~~ uf2
ford ~~ ford
'
meas.fit2 <- lavaan(meas.mod2, sample.cov = cov.mat, sample.nobs = N)
summary(meas.fit)
summary(meas.fit2)
