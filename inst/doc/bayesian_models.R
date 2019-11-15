## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

## ---- warning=FALSE, message=FALSE---------------------------------------
library(ppcor)

df <- iris[, 1:4]  # Remove the Species factor

ppcor::pcor(df)$estimate[2:4, 1]  # Select the rows of interest

## ---- warning=FALSE, message=FALSE---------------------------------------
library(effectsize)
library(parameters)

model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = df) 

parameters <- model_parameters(model)[2:4,]
convert_t_to_r(parameters$t, parameters$df_residual)

## ---- warning=FALSE, message=FALSE---------------------------------------
standardize_parameters(model)$Std_Coefficient[2:4]

## ---- warning=FALSE, message=FALSE, eval = FALSE-------------------------
#  library(rstanarm)
#  
#  model <- stan_glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = df)

## ---- warning=FALSE, message=FALSE, echo = FALSE-------------------------
library(rstanarm)
model <- stan_glm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = df, refresh = 0, chains = 2) 

## ---- warning=FALSE, message=FALSE, eval=FALSE---------------------------
#  library(bayestestR)
#  
#  r <- convert_posteriors_to_r(model)
#  bayestestR::describe_posterior(r)$Median[2:4]

## ---- warning=FALSE, message=FALSE, eval=FALSE---------------------------
#  model <- glm(vs ~ cyl + disp + drat, data = mtcars, family = "binomial")
#  
#  parameters <- model_parameters(model)
#  parameters$r <- convert_z_to_r(parameters$z, n = insight::n_obs(model))
#  parameters

## ---- warning=FALSE, message=FALSE, eval=FALSE---------------------------
#  parameters$r_from_odds <- convert_odds_to_r(parameters$Coefficient, log = TRUE)
#  parameters

## ---- warning=FALSE, message=FALSE, eval = FALSE-------------------------
#  model <- stan_glm(vs ~ cyl + disp + drat,, data = mtcars, family = "binomial")

## ---- warning=FALSE, message=FALSE, echo = FALSE, eval=FALSE-------------
#  model <- stan_glm(vs ~ cyl + disp + drat,, data = mtcars, family = "binomial", refresh = 0)

## ---- warning=FALSE, message=FALSE, eval=FALSE---------------------------
#  parameters <- model_parameters(model)
#  r <- convert_posteriors_to_r(model)
#  parameters$r <- bayestestR::describe_posterior(r)$Median
#  parameters

## ---- warning=FALSE, message=FALSE, eval=FALSE---------------------------
#  parameters <- model_parameters(model)
#  r <- convert_posteriors_to_r(model)
#  parameters$r <- bayestestR::describe_posterior(r)$Median
#  parameters$r_from_odds <- convert_odds_to_r(parameters$Median, log = TRUE)
#  parameters

