## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
options(knitr.kable.NA = "")
knitr::opts_chunk$set(comment = ">")
options(digits = 3)

set.seed(7)

## -----------------------------------------------------------------------------
library(effectsize)
options(es.use_symbols = TRUE) # get nice symbols when printing! (On Windows, requires R >= 4.2.0)

## -----------------------------------------------------------------------------
t.test(mpg ~ am, data = mtcars, var.equal = TRUE)

cohens_d(mpg ~ am, data = mtcars)

## -----------------------------------------------------------------------------
hedges_g(mpg ~ am, data = mtcars)

## -----------------------------------------------------------------------------
t.test(mpg ~ am, data = mtcars, var.equal = FALSE)

cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)

hedges_g(mpg ~ am, data = mtcars, pooled_sd = FALSE)

## -----------------------------------------------------------------------------
glass_delta(mpg ~ am, data = mtcars)

## -----------------------------------------------------------------------------
t.test(mpg ~ am, data = mtcars, var.equal = TRUE, alternative = "less")

cohens_d(mpg ~ am, data = mtcars, pooled_sd = TRUE, alternative = "less")

## -----------------------------------------------------------------------------
t.test(mtcars$wt, mu = 2.7)

cohens_d(mtcars$wt, mu = 2.7)

hedges_g(mtcars$wt, mu = 2.7)

## -----------------------------------------------------------------------------
t.test(extra ~ group, data = sleep, paired = TRUE)

cohens_d(extra ~ group, data = sleep, paired = TRUE)

hedges_g(extra ~ group, data = sleep, paired = TRUE)

## ---- eval = requireNamespace("BayesFactor", quietly = TRUE), message=FALSE----
library(BayesFactor)
BFt <- ttestBF(formula = mpg ~ am, data = mtcars)

effectsize(BFt, type = "d")

## -----------------------------------------------------------------------------
mahalanobis_d(mpg + hp + cyl ~ am, data = mtcars)

## ---- warning=FALSE-----------------------------------------------------------
A <- c(48, 48, 77, 86, 85, 85)
B <- c(14, 34, 34, 77)

wilcox.test(A, B) # aka Mann–Whitney U test

rank_biserial(A, B)

## -----------------------------------------------------------------------------
x <- c(1.15, 0.88, 0.90, 0.74, 1.21, 1.36, 0.89)

wilcox.test(x, mu = 1) # aka Signed-Rank test

rank_biserial(x, mu = 1)

## -----------------------------------------------------------------------------
x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.88, 0.65, 0.60, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

wilcox.test(x, y, paired = TRUE) # aka Signed-Rank test

rank_biserial(x, y, paired = TRUE)

## -----------------------------------------------------------------------------
cohens_u1(mpg ~ am, data = mtcars)

p_overlap(mpg ~ am, data = mtcars)

## -----------------------------------------------------------------------------
p_overlap(mpg ~ am, data = mtcars, parametric = FALSE)

## -----------------------------------------------------------------------------
p_superiority(mpg ~ am, data = mtcars)

## -----------------------------------------------------------------------------
cohens_u2(mpg ~ am, data = mtcars)

cohens_u3(mpg ~ am, data = mtcars)

## -----------------------------------------------------------------------------
p_superiority(mpg ~ am, data = mtcars, parametric = FALSE)

cohens_u2(mpg ~ am, data = mtcars, parametric = FALSE)

cohens_u3(mpg ~ am, data = mtcars, parametric = FALSE)

## -----------------------------------------------------------------------------
p_superiority(mtcars$wt, mu = 2.75)

p_superiority(mtcars$wt, mu = 2.75, parametric = FALSE)

## -----------------------------------------------------------------------------
p_superiority(extra ~ group, data = sleep, 
              paired = TRUE, mu = -1)

p_superiority(extra ~ group, data = sleep, 
              paired = TRUE, mu = -1, 
              parametric = FALSE)

## ---- eval = requireNamespace("BayesFactor", quietly = TRUE)------------------
effectsize(BFt, type = "p_superiority")

effectsize(BFt, type = "u1")

effectsize(BFt, type = "u2")

effectsize(BFt, type = "u3")

effectsize(BFt, type = "overlap")

