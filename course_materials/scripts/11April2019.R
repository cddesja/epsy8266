library(lavaan)
alcuse.xbar <- c(0.92, 0.76, 0.73, 0.62, 0.08, 0.08, 0.08, 0.07, 22.69, 0.54, 0.30,
          0.14, 12.72)
alcuse.sigma <- c(1.68, 1.35, 1.26, 1.08, 0.27, 0.27, 0.27, 0.26, 1.38, 0.49, 0.46,
           0.35, 2.02)
lower.cor <- '
1.00
.494 1.00
.440 .519 1.00
.382 .471 .510 1.00
-.074 -.068 -.062 -.035 1.00
-.023 -.048 -.057 -.055 -.086 1.00
.009 -.003 -.036 -.039 -.085 -.083  1.00
.043 .025 .011 -.022 -.081 -.080 -.079 1.00
.032 .020 -.005 .010 .050 .028 .007 .028 1.00
.231, .238 .252 .264 -.048 -.005 -.028 -.003 .011 1.00
-.142 -.146 -.120 -.118 -.085 -.080 -.067 -.071 -.014 -.013 1.00
-.025 -.014 -.028 -.001 .012 .019 -.021 -.013 -.023 .040 -.265 1.00
.012 -.005 -.021 -.018 .006 .006 .009 .049 .222 -.110 -.149 -.125 1.00'
alcuse.cov <- getCov(lower.cor,
                     names = c(paste0("alc", 1:4), paste0("mar", 1:4),
                               "age", "male", "black", "hispanic", "education"),
                     sds = alcuse.sigma)


## ------------------------------------------------------------------------
# Read in data in wide format
baldwin_data <- read.csv("https://bit.ly/2G7ibPf")
# y1: depression
# y2: quality of life

# need to make even wider for lavaan``
baldwin_wide <- reshape(baldwin_data,
                        direction = "wide",
                        v.names = c("y1", "y2"),
                        timevar = "time",
                        idvar = "pid") 
colnames(baldwin_wide) <- c("pid", "tx", 
                            paste0(rep(c("dep", "qol"), 3), rep(0:2, each = 2)))

