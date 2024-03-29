test_that("print | effectsize table", {
  ## digits
  d <- cohens_d(1:4, c(1, 1:5))
  expect_output(print(d), "[-1.37, 1.16]", fixed = TRUE)
  expect_output(print(d, digits = 4), "[-1.3730, 1.1595]", fixed = TRUE)
  expect_output(print(d, digits = "signif4"), "[-1.373, 1.16]", fixed = TRUE)
  expect_output(print(d, digits = "scientific4"), "[-1.3730e+00, 1.1595e+00]", fixed = TRUE)

  ## alternative + rounded bound
  RCT <- matrix(c(71, 30, 31, 13, 50, 100, 4, 5, 7), nrow = 3, byrow = TRUE)
  V1 <- cramers_v(RCT)
  V2 <- cramers_v(RCT, alternative = "two")
  w <- cohens_w(RCT)

  expect_output(print(V1), regexp = "[1.00]", fixed = TRUE)
  expect_output(print(V1, digits = "signif4"), regexp = "[1]", fixed = TRUE)
  expect_output(print(V1, digits = "scientific2"), regexp = "[1.00e+00]", fixed = TRUE)
  expect_error(expect_output(print(V2), regexp = "fixed"))
  expect_output(print(w), regexp = "[1.41~]", fixed = TRUE)
  expect_output(print(w, digits = "signif4"), regexp = "[1.414]", fixed = TRUE)
  expect_output(print(w, digits = "scientific2"), regexp = "[1.41e+00]", fixed = TRUE)


  ## Column name
  expect_output(print(d), "Cohen's d")
  expect_output(print(V1), "Cramer's V")
  expect_output(print(w), "Cohen's w")


  ## Interpretation
  d_ <- interpret(d, rules = "sawilowsky2009")
  expect_output(print(d_), regexp = "sawilowsky2009")
  expect_output(print(d_), regexp = "Interpretation")

  V1_ <- interpret(V1, rules = "funder2019")
  expect_output(print(V1_), regexp = "funder2019")
  expect_output(print(V1_), regexp = "Interpretation")

  w_ <- interpret(w, rules = "funder2019")
  expect_output(print(w_), regexp = "funder2019")
  expect_output(print(w_), regexp = "Interpretation")

  ## md or html
  skip_if_not_installed("gt")
  skip_if_not_installed("knitr")
  expect_s3_class(print_html(d), "gt_tbl")
  expect_s3_class(print_md(d), "knitr_kable")
})

test_that("print | effectsize_difference", {
  ## Pooled
  d1 <- cohens_d(1:3, c(1, 1:3))
  expect_error(expect_output(print(d1), regexp = "Deviation from a difference"))
  expect_output(print(d1), regexp = " pooled", fixed = TRUE)

  ## Un-pooled and mu
  d2 <- cohens_d(1:3, c(1, 1:3), pooled_sd = FALSE, mu = -1)
  expect_output(print(d2), regexp = "Deviation from a difference of -1")
  expect_output(print(d2), regexp = "un-pooled", fixed = TRUE)

  ## paired
  d3 <- cohens_d(1:5, c(1, 1:4), paired = TRUE)
  expect_error(expect_output(print(d3), regexp = "Deviation from a difference"))
  expect_error(expect_output(print(d3), regexp = "pooled"))

  ## paired and mu
  d4 <- cohens_d(1:5, c(1, 1:4), paired = TRUE, mu = 0.1)
  expect_output(print(d4), regexp = "Deviation from a difference of 0.1")

  ## CLSE
  expect_output(print(d1, append_CLES = TRUE), regexp = "U3")
  expect_error(expect_output(print(d2, append_CLES = TRUE), regexp = "U3"))
})

test_that("print | effectsize_anova", {
  a <- aov(mpg ~ cyl + gear, mtcars)

  e1 <- eta_squared(a)
  expect_output(print(e1), regexp = "(Type I)", fixed = TRUE)
  expect_output(print(e1), regexp = "Eta2 (partial)", fixed = TRUE)
  expect_output(print(e1), regexp = "One-sided CIs: upper bound fixed at [1.00]", fixed = TRUE)

  e2 <- eta_squared(a, generalized = "gear")
  expect_output(print(e2), regexp = "Observed variables: gear", fixed = TRUE)

  e3 <- eta_squared(a, generalized = TRUE)
  expect_output(print(e3), regexp = "Observed variables: All", fixed = TRUE)

  skip_if_not_installed("car")
  A <- car::Anova(a, type = 3)
  e4 <- eta_squared(A)
  expect_output(print(e4), regexp = "(Type III)", fixed = TRUE)
})

test_that("print | equivalence_test_effectsize", {
  d <- cohens_d(1:3, c(1, 1:3))

  equtest <- equivalence_test(d)
  expect_output(print(equtest), regexp = "ROPE: [-0.10, 0.10]", fixed = TRUE)

  equtest2 <- equivalence_test(d, rule = "cet")
  expect_output(print(equtest2), regexp = "Conditional")

  equtest3 <- equivalence_test(d, rule = "bayes")
  expect_output(print(equtest3), regexp = "Using Bayesian guidlines", fixed = TRUE)
})

# Rules -----------
test_that("rules", {
  skip_on_cran()
  r1 <- rules(1:3, letters[1:4], name = "XX")
  expect_output(print(r1), regexp = "Thresholds")
  expect_output(print(r1), regexp = "<=")
  expect_output(print(r1), regexp = "XX")
  expect_output(print(r1), regexp = "1 <   b   <= 2")
  expect_output(print(r1, digits = "scientific1"),
    regexp = "2.0e+00 <   c   <= 3.0e+00", fixed = TRUE
  )


  r2 <- rules(c(1, 2, 3.1), letters[1:3], name = "YY")
  expect_output(print(r2), regexp = "Values")
  expect_output(print(r2), regexp = "YY")
  expect_output(print(r2), regexp = "b ~ 2")
  expect_output(print(r2), regexp = "c ~ 3.1")
  expect_output(print(r2, digits = "signif3"), regexp = "c ~ 3.1")
  expect_output(print(r2, digits = "scientific1"), regexp = "a ~ 1.0e+00", fixed = TRUE)


  expect_output(print(interpret(0, r1)), '"a"')
  expect_output(print(interpret(0, r1)), "XX")
})



test_that("printing symbols works as expected", {
  skip_if_not(l10n_info()[["UTF-8"]])

  RCT <- matrix(c(71, 50, 30, 100), nrow = 2L)
  P <- phi(RCT)

  expect_output(print(P, use_symbols = TRUE), "\u03D5")
})
