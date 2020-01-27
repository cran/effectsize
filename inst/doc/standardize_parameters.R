## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
knitr::opts_chunk$set(comment = ">")
options(digits = 2)
options(knitr.kable.NA = '')

if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("parameters", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

set.seed(333)

## ---- warning=FALSE, message=FALSE, eval=FALSE--------------------------------
#  library(effectsize)
#  library(dplyr)
#  
#  lm(Sepal.Length ~ Petal.Length, data = iris) %>%
#    standardize_parameters()

## ---- warning=FALSE, message=FALSE, echo = FALSE------------------------------
library(effectsize)
library(dplyr)

lm(Sepal.Length ~ Petal.Length, data = iris) %>% 
  standardize_parameters() %>% 
  knitr::kable(digits = 2)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(parameters)

cor.test(iris$Sepal.Length, iris$Petal.Length) %>% 
  model_parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
if (require("ppcor")) {
  df <- iris[, 1:4]  # Remove the Species factor
  ppcor::pcor(df)$estimate[2:4, 1]  # Select the rows of interest
}

## ---- warning=FALSE, message=FALSE--------------------------------------------
model <- lm(Sepal.Length ~ ., data = df) 

parameters <- model_parameters(model)[2:4,]
convert_t_to_r(parameters$t, parameters$df_residual)

## ---- warning=FALSE, message=FALSE, eval=FALSE--------------------------------
#  model %>%
#    standardize_parameters()

## ---- warning=FALSE, message=FALSE, echo = FALSE------------------------------
model %>% 
  standardize_parameters()  %>% 
  knitr::kable(digits = 2)

## ---- warning=FALSE, message=FALSE, eval=FALSE--------------------------------
#  lm(Sepal.Length ~ Species, data = iris) %>%
#    standardize_parameters()

## ---- warning=FALSE, message=FALSE, echo = FALSE------------------------------
lm(Sepal.Length ~ Species, data = iris) %>% 
  standardize_parameters() %>% 
  knitr::kable(digits = 2)

## ---- warning=FALSE, message=FALSE--------------------------------------------
# Select portion of data containing the two levels of interest
data <- iris[iris$Species %in% c("setosa", "versicolor"), ]

cohens_d(Sepal.Length ~ Species, data = data) 

## ---- warning=FALSE, message=FALSE, eval=FALSE--------------------------------
#  lm(Sepal.Length ~ Species, data = data) %>%
#    standardize_parameters()

## ---- warning=FALSE, message=FALSE, echo = FALSE------------------------------
lm(Sepal.Length ~ Species, data = data) %>% 
  standardize_parameters() %>% 
  knitr::kable(digits = 2)

## ---- warning=FALSE, message=FALSE--------------------------------------------
cohens_d(Sepal.Length ~ Species, data = data, pooled_sd = FALSE) 

