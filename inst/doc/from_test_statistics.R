## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
library(effectsize)

knitr::opts_chunk$set(comment = ">")
options(digits = 2)
options(knitr.kable.NA = "")

set.seed(747)

## ---- eval = requireNamespace("afex", quietly = TRUE), message=FALSE----------
library(afex)

data(md_12.1)

aov_fit <- aov_car(rt ~ angle * noise + Error(id / (angle * noise)),
  data = md_12.1,
  anova_table = list(correction = "none", es = "pes")
)
aov_fit

## -----------------------------------------------------------------------------
library(effectsize)
options(es.use_symbols = TRUE) # get nice symbols when printing! (On Windows, requires R >= 4.2.0)

F_to_eta2(
  f = c(40.72, 33.77, 45.31),
  df = c(2, 1, 2),
  df_error = c(18, 9, 18)
)

## ---- eval = requireNamespace("afex", quietly = TRUE) && requireNamespace("emmeans", quietly = TRUE)----
library(emmeans)

joint_tests(aov_fit, by = "noise")

F_to_eta2(
  f = c(5, 79),
  df = 2,
  df_error = 29
)

## ---- eval = requireNamespace("afex", quietly = TRUE) && requireNamespace("emmeans", quietly = TRUE)----
pairs(emmeans(aov_fit, ~angle))

t_to_eta2(
  t = c(-6.2, -8.2, -3.3),
  df_error = 9
)

## ---- eval = requireNamespace("lmerTest", quietly = TRUE), message=FALSE------
library(lmerTest)

fit_lmm <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

anova(fit_lmm)

F_to_eta2(45.8, 1, 17)

## ---- eval = requireNamespace("lmerTest", quietly = TRUE)---------------------
parameters::model_parameters(fit_lmm, effects = "fixed", ci_method = "satterthwaite")

t_to_eta2(6.77, df_error = 17)

## -----------------------------------------------------------------------------
F_to_eta2(45.8, 1, 17)
F_to_epsilon2(45.8, 1, 17)
F_to_omega2(45.8, 1, 17)

## ---- eval = requireNamespace("lmerTest", quietly = TRUE)---------------------
parameters::model_parameters(fit_lmm, effects = "fixed", ci_method = "satterthwaite")

t_to_r(6.77, df_error = 17)

## -----------------------------------------------------------------------------
fit_lm <- lm(rating ~ complaints + critical, data = attitude)

parameters::model_parameters(fit_lm)

t_to_r(
  t = c(7.46, 0.01),
  df_error = 27
)

## ---- eval=require(correlation, quietly = TRUE)-------------------------------
correlation::correlation(attitude[, c(1, 2, 6)], partial = TRUE)[1:2, c(2, 3, 7, 8)]

## ---- eval = requireNamespace("afex", quietly = TRUE) && requireNamespace("emmeans", quietly = TRUE)----
pairs(emmeans(aov_fit, ~angle))

t_to_r(
  t = c(-5.7, -8.9, -3.2),
  df_error = 18
)

## ---- eval = requireNamespace("emmeans", quietly = TRUE)----------------------
m <- lm(breaks ~ tension, data = warpbreaks)

em_tension <- emmeans(m, ~tension)
pairs(em_tension)

t_to_d(
  t = c(2.53, 3.72, 1.20),
  df_error = 51
)

## ---- eval = requireNamespace("emmeans", quietly = TRUE)----------------------
eff_size(em_tension, sigma = sigma(m), edf = df.residual(m))

## ---- eval = requireNamespace("afex", quietly = TRUE) && requireNamespace("emmeans", quietly = TRUE)----
pairs(emmeans(aov_fit, ~angle))

t_to_d(
  t = c(-5.7, -5.9, -3.2),
  df_error = 18,
  paired = TRUE
)

