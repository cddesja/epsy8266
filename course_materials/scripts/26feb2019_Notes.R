subj <- c("gaelic", "english", "history", "math", "algebra", "geometry")
sch.subj <- matrix(c(1, .44, .41, .29, .33, .25,
                     .44, 1, .35, .35, .32, .33,
                     .41, .35, 1, .16, .19, .18,
                     .29, .35, .16, 1, .59, .47,
                     .33, .32, .19, .59, 1, .46,
                     .25, .33, .18, .47, .46, 1),
                   nrow = 6,
                   ncol = 6)
colnames(sch.subj) <- subj
rownames(sch.subj) <- subj
sch.subj

## ----fig.height=4--------------------------------------------------------
plot(1:6, eigen(sch.subj)$values, type = "b",
     xlab = "Number of factors", ylab = "Eigenvalues", pch = 16)
abline(h = 1, col = "gray")

## ----size = "tiny"-------------------------------------------------------
factanal(covmat = sch.subj, n.obs = 220, factors = 1)

## ----size = "tiny"-------------------------------------------------------
factanal(covmat = sch.subj, n.obs = 220, factors = 2)

## ----size = "tiny"-------------------------------------------------------
factanal(covmat = sch.subj, n.obs = 220, factors = 2, rotation = "promax")

## ----eval = FALSE--------------------------------------------------------
## # Define constrains for effects-coding
## lam1 == 7 - lam2 - lam3 - lam4 - lam5 - lam6 - lam7

