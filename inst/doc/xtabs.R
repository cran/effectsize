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
(MPG_Gear <- table(mtcars$mpg < 20, mtcars$vs))

phi(MPG_Gear, adjust = FALSE)

# Same as:
cor(mtcars$mpg < 20, mtcars$vs)

## -----------------------------------------------------------------------------
pearsons_c(MPG_Gear)

## -----------------------------------------------------------------------------
data("food_class")
food_class

## -----------------------------------------------------------------------------
cramers_v(food_class, adjust = FALSE)

tschuprows_t(food_class)

## -----------------------------------------------------------------------------
data("Music_preferences2")
Music_preferences2


chisq.test(Music_preferences2)

cramers_v(Music_preferences2)

tschuprows_t(Music_preferences2)

pearsons_c(Music_preferences2)

cohens_w(Music_preferences2) # > 1

## ---- eval = requireNamespace("BayesFactor", quietly = TRUE), message=FALSE----
library(BayesFactor)
BFX <- contingencyTableBF(MPG_Gear, sampleType = "jointMulti")

effectsize(BFX, type = "phi") # for 2 * 2


BFX <- contingencyTableBF(Music_preferences2, sampleType = "jointMulti")

effectsize(BFX, type = "cramers_v")

effectsize(BFX, type = "tschuprows_t")

effectsize(BFX, type = "cohens_w")

effectsize(BFX, type = "pearsons_c")

## -----------------------------------------------------------------------------
O <- c(89, 37, 130, 28, 2) # observed group sizes
E <- c(.40, .20, .20, .15, .05) # expected group freq

chisq.test(O, p = E)

pearsons_c(O, p = E)

cohens_w(O, p = E)

## -----------------------------------------------------------------------------
fei(O, p = E)

# Observed perfectly matches Expected
(O1 <- c(E * 286))

fei(O1, p = E) 


# Observed deviates maximally from Expected:
# All observed values are in the least expected class!
(O2 <- c(rep(0, 4), 286))

fei(O2, p = E)

## -----------------------------------------------------------------------------
data("RCT_table")
RCT_table

chisq.test(RCT_table) # or fisher.test(RCT_table)

oddsratio(RCT_table)

## -----------------------------------------------------------------------------
riskratio(RCT_table)

## -----------------------------------------------------------------------------
cohens_h(RCT_table)

## ---- eval = requireNamespace("BayesFactor", quietly = TRUE)------------------
BFX <- contingencyTableBF(RCT_table, sampleType = "jointMulti")

effectsize(BFX, type = "or")

effectsize(BFX, type = "rr")

effectsize(BFX, type = "cohens_h")

## -----------------------------------------------------------------------------
data("screening_test")

phi(screening_test$Diagnosis, screening_test$Test1)

phi(screening_test$Diagnosis, screening_test$Test2)

## -----------------------------------------------------------------------------
tests <- table(Test1 = screening_test$Test1, Test2 = screening_test$Test2)
tests

mcnemar.test(tests)

cohens_g(tests)

