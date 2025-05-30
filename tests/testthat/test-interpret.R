# interpret generic ----
test_that("interpret generic", {
  rules_grid <- rules(c(0.01, 0.05), c("very significant", "significant", "not significant"))
  expect_identical(interpret(0.001, rules_grid)[1], "very significant")
  expect_identical(interpret(0.021, rules_grid)[1], "significant")
  expect_identical(interpret(0.08, rules_grid)[1], "not significant")
  expect_identical(
    interpret(c(0.01, 0.005, 0.08), rules_grid)[1:3],
    c("very significant", "very significant", "not significant")
  )
  expect_error(rules(0.5, c("A", "B", "C")), "Too many")
  expect_error(rules(c(0.5, 0.2, 0.7), c("A", "B", "C", "D")), "sorted")

  expect_error(rules(1), NA)
  expect_error(rules("a"), "must be numeric")


  r1 <- rules(c(0, 1), labels = c("some", "few", "many"))
  r2 <- rules(c(0, 1), labels = c("some", "few", "many"), right = FALSE)

  expect_equal(interpret(c(0, 1), r1)[], c("some", "few"), ignore_attr = TRUE)
  expect_equal(interpret(c(0, 1), r2)[], c("few", "many"), ignore_attr = TRUE)
})


test_that("interpret matrix / array", {
  # Matrix
  r <- cor(mtcars[, 8:11])
  out <- interpret_r(r)

  expect_identical(dim(out), dim(r))
  expect_identical(dimnames(out), dimnames(r))

  out2 <- interpret_r(as.vector(r))
  expect_equal(out2, as.vector(out), ignore_attr = TRUE)

  # Array
  set.seed(111)
  p <- array(runif(3 * 2 * 4, max = 0.1), dim = c(3, 2, 4))
  out <- interpret_p(p, rules = "rss")

  expect_identical(dim(out), dim(p))

  out2 <- interpret_p(as.vector(p), rules = "rss")
  expect_equal(out2, as.vector(out), ignore_attr = TRUE)
})

# interpret types ----
test_that("interpret_r", {
  expect_identical(interpret_r(0.21)[1], "medium")
  expect_identical(interpret_r(0.21, "cohen1988")[1], "small")
  expect_identical(interpret_r(0.21, "lovakov2021")[1], "small")
  expect_identical(interpret_r(0.7, "evans1996")[1], "strong")
  expect_identical(interpret_r(c(0.5, -0.08), "cohen1988")[1:2], c("large", "very small"))
  expect_identical(interpret_r(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_r(0.6, "DUPA"), "must be")
})



test_that("interpret_p", {
  expect_identical(interpret_p(0.021)[1], "significant")
  expect_identical(interpret_p(0.08)[1], "not significant")
  expect_identical(interpret_p(c(0.01, 0.08))[1:2], c("significant", "not significant"))
  expect_identical(interpret_p(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_p(0.6, "DUPA"), "must be")
})


test_that("interpret_direction", {
  expect_identical(interpret_direction(c(0.01, -0.08))[1:2], c("positive", "negative"))
})


test_that("interpret_cohens_d", {
  expect_identical(interpret_cohens_d(0.021)[1], "very small")
  expect_identical(interpret_cohens_d(1.3, "sawilowsky2009")[1], "very large")
  expect_identical(interpret_cohens_d(c(0.45, 0.85), "cohen1988")[1:2], c("small", "large"))
  expect_identical(interpret_cohens_d(c(0.45, 0.85), "lovakov2021")[1:2], c("medium", "large"))
  expect_identical(interpret_cohens_d(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_cohens_d(0.6, "DUPA"), "must be")
})

test_that("interpret_cohens_g", {
  expect_identical(interpret_cohens_g(0.021)[1], "very small")
  expect_identical(interpret_cohens_g(c(0.10, 0.35), "cohen1988")[1:2], c("small", "large"))
  expect_identical(interpret_cohens_g(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_cohens_g(0.6, "DUPA"), "must be")
})


test_that("interpret_rope", {
  expect_identical(interpret_rope(0, ci = 0.9)[1], "significant")
  expect_identical(interpret_rope(c(0.50, 1), ci = 0.9)[1:2], c("undecided", "negligible"))
  expect_identical(interpret_rope(c(0.98, 0.991), ci = 1)[1:2], c("probably negligible", "negligible"))
  expect_identical(interpret_rope(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_rope(0.6, "DUPA"), "must be")
})


test_that("interpret_oddsratio", {
  # Chen 2010, table 1 row 6
  OR <- c(1.4, 2.5, 4.4, 7.5)
  p0 <- 0.06
  expect_equal(interpret_oddsratio(OR, p0 = p0), c("very small", "small", "medium", "large"), ignore_attr = TRUE)
  expect_equal(interpret_oddsratio(OR), c("very small", "medium", "large", "large"), ignore_attr = TRUE)

  expect_equal(interpret_oddsratio(
    c(0.1, 0.5, 2, 10),
    rules(3, c("A", "B"))
  ), c("B", "A", "A", "B"), ignore_attr = TRUE)
})


test_that("interpret_r2", {
  expect_identical(interpret_r2(0.4)[1], "substantial")
  expect_identical(interpret_r2(c(0, 0.4), "falk1992")[1:2], c("negligible", "adequate"))
  expect_identical(interpret_r2(c(0.1, 0.4), "chin1998")[1:2], c("very weak", "moderate"))
  expect_identical(interpret_r2(c(0.1, 0.4), "hair2011")[1:2], c("very weak", "weak"))
  expect_identical(interpret_r2(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_r2(0.6, "DUPA"), "must be")
})


test_that("interpret_bf", {
  expect_error(interpret_bf(-2), "Negative")
  expect_identical(interpret_bf(1)[1], "no evidence against or in favour of")
  expect_identical(
    interpret_bf(c(0.8, 3.5), "jeffreys1961")[1:2],
    c("anecdotal evidence against", "moderate evidence in favour of")
  )
  expect_identical(
    interpret_bf(c(0.8, 3.5), "raftery1995")[1:2],
    c("weak evidence against", "positive evidence in favour of")
  )
  expect_identical(interpret_bf(2, rules(0.5, c("A", "B")))[1], "B evidence in favour of")
  expect_error(interpret_bf(2, "DUPA"), "must be")

  skip_on_cran() # just in case there are changes in insight
  bf <- c(10^seq(-4, 4), NA)
  expect_identical(interpret_bf(bf, include_value = TRUE, protect_ratio = TRUE, exact = TRUE),
    c(
      "extreme evidence (BF = 1/1.00e+04) against", "extreme evidence (BF = 1/1000.00) against",
      "very strong evidence (BF = 1/100.00) against", "moderate evidence (BF = 1/10.00) against",
      "no evidence (BF = 1.00) against or in favour of", "strong evidence (BF = 10.00) in favour of",
      "extreme evidence (BF = 100.00) in favour of", "extreme evidence (BF = 1000.00) in favour of",
      "extreme evidence (BF = 1.00e+04) in favour of", ""
    ),
    ignore_attr = TRUE
  )
})



test_that("interpret_omega_squared", {
  expect_identical(interpret_omega_squared(0.1)[1], "medium")
  expect_identical(interpret_omega_squared(c(0.1, 0.25))[1:2], c("medium", "large"))
  expect_identical(interpret_omega_squared(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_omega_squared(0.6, "DUPA"), "must be")

  # these should be same
  expect_identical(interpret_eta_squared(0.1)[1], interpret_omega_squared(0.1)[1])
  expect_identical(
    interpret_eta_squared(c(0.1, 0.25))[1:2],
    interpret_omega_squared(c(0.1, 0.25))[1:2]
  )
})

test_that("interpret_kendalls_w", {
  expect_identical(interpret_kendalls_w(0.1)[1], "slight agreement")
  expect_identical(
    interpret_kendalls_w(c(0.1, 0.25))[1:2],
    c("slight agreement", "fair agreement")
  )
  expect_identical(interpret_kendalls_w(0.9)[1], "almost perfect agreement")
  expect_identical(interpret_kendalls_w(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_kendalls_w(0.6, "DUPA"), "must be")
})


test_that("interpret_rhat", {
  expect_identical(interpret_rhat(1)[1], "converged")
  expect_identical(interpret_rhat(c(1, 1.02))[1:2], c("converged", "failed"))
  expect_identical(interpret_rhat(c(1, 1.02), "gelman1992")[1:2], c("converged", "converged"))
  expect_identical(interpret_rhat(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_rhat(0.6, "DUPA"), "must be")
})


test_that("interpret_ess", {
  expect_identical(interpret_ess(1000)[1], "sufficient")
  expect_identical(interpret_ess(c(1000, 800))[1:2], c("sufficient", "insufficient"))
  expect_identical(interpret_ess(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_ess(0.6, "DUPA"), "must be")
})


test_that("interpret_fit", {
  expect_equal(interpret_gfi(c(0.5, 0.99)), c("poor", "satisfactory"), ignore_attr = TRUE)
  expect_equal(interpret_agfi(c(0.5, 0.99)), c("poor", "satisfactory"), ignore_attr = TRUE)
  expect_equal(interpret_nfi(c(0.5, 0.99)), c("poor", "satisfactory"), ignore_attr = TRUE)
  expect_equal(interpret_nnfi(c(0.5, 0.99)), c("poor", "satisfactory"), ignore_attr = TRUE)
  expect_equal(interpret_cfi(c(0.5, 0.99)), c("poor", "satisfactory"), ignore_attr = TRUE)
  expect_equal(interpret_rfi(c(0.5, 0.99)), c("poor", "satisfactory"), ignore_attr = TRUE)
  expect_equal(interpret_ifi(c(0.5, 0.99)), c("poor", "satisfactory"), ignore_attr = TRUE)
  expect_equal(interpret_pnfi(c(0.5, 0.99)), c("poor", "satisfactory"), ignore_attr = TRUE)
  expect_equal(interpret_rmsea(c(0.1, 0.05)), c("poor", "satisfactory"), ignore_attr = TRUE)
  expect_equal(interpret_srmr(c(0.1, 0.05)), c("poor", "satisfactory"), ignore_attr = TRUE)

  cr <- rules(0.5, c("A", "B"))
  expect_equal(interpret_gfi(0.6, cr), "B", ignore_attr = TRUE)
  expect_equal(interpret_agfi(0.6, cr), "B", ignore_attr = TRUE)
  expect_equal(interpret_nfi(0.6, cr), "B", ignore_attr = TRUE)
  expect_equal(interpret_nnfi(0.6, cr), "B", ignore_attr = TRUE)
  expect_equal(interpret_cfi(0.6, cr), "B", ignore_attr = TRUE)
  expect_equal(interpret_rfi(0.6, cr), "B", ignore_attr = TRUE)
  expect_equal(interpret_ifi(0.6, cr), "B", ignore_attr = TRUE)
  expect_equal(interpret_pnfi(0.6, cr), "B", ignore_attr = TRUE)
  expect_equal(interpret_rmsea(0.6, cr), "B", ignore_attr = TRUE)
  expect_equal(interpret_srmr(0.6, cr), "B", ignore_attr = TRUE)

  expect_error(interpret_gfi(0.6, "DUPA"), "must be")
  expect_error(interpret_agfi(0.6, "DUPA"), "must be")
  expect_error(interpret_nfi(0.6, "DUPA"), "must be")
  expect_error(interpret_nnfi(0.6, "DUPA"), "must be")
  expect_error(interpret_cfi(0.6, "DUPA"), "must be")
  expect_error(interpret_rfi(0.6, "DUPA"), "must be")
  expect_error(interpret_ifi(0.6, "DUPA"), "must be")
  expect_error(interpret_pnfi(0.6, "DUPA"), "must be")
  expect_error(interpret_rmsea(0.6, "DUPA"), "must be")
  expect_error(interpret_srmr(0.6, "DUPA"), "must be")

  skip_on_cran()
  skip_if_not_installed("lavaan")
  skip_if_not_installed("performance")

  structure <- " ind60 =~ x1 + x2 + x3
                   dem60 =~ y1 + y2 + y3
                   dem60 ~ ind60 "
  model <- lavaan::sem(structure, data = lavaan::PoliticalDemocracy)
  int <- interpret(model)
  expect_identical(int$Name, c("GFI", "AGFI", "NFI", "NNFI", "CFI", "RMSEA", "SRMR", "RFI", "PNFI", "IFI"))
  expect_equal(int$Value, c(0.9666, 0.9124, 0.9749, 1.0001, 1, 0, 0.0273, 0.9529, 0.5199, 1.0001), tolerance = 0.001)

  int2 <- interpret(performance::model_performance(model))
  expect_identical(int, int2)
})

test_that("interpret_icc", {
  expect_equal(interpret_icc(c(0.45, 0.55, 0.8, 0.95)), c("poor", "moderate", "good", "excellent"), ignore_attr = TRUE)
  expect_identical(interpret_icc(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_icc(0.6, "DUPA"), "must be")
})

test_that("interpret_vif", {
  expect_equal(interpret_vif(c(1, 5.5, 10)), c("low", "moderate", "high"), ignore_attr = TRUE)
  expect_identical(interpret_icc(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_icc(0.6, "DUPA"), "must be")
})

test_that("interpret_pd", {
  expect_equal(interpret_pd(c(0.9, 0.99)), c("not significant", "significant"), ignore_attr = TRUE)
  expect_equal(interpret_pd(c(0.9, 0.99), "makowski2019"), c("uncertain", "likely existing"), ignore_attr = TRUE)
  expect_identical(interpret_pd(0.6, rules(0.5, c("A", "B")))[1], "B")
  expect_error(interpret_pd(0.6, "DUPA"), "must be")
})

# interpret effectsize_table ----
test_that("interpret effectsize_table", {
  d <- cohens_d(mpg ~ am, data = mtcars)
  d_ <- interpret(d, rules = "cohen1988")
  expect_equal(d_[["Interpretation"]], "large", ignore_attr = TRUE)
  expect_s3_class(d_[["Interpretation"]], "effectsize_interpret")
  expect_output(print(d_), "large")
  expect_output(print(d_), "Interpretation rule: cohen1988")


  V <- cramers_v(matrix(c(71, 30, 50, 100), 2))
  V_ <- interpret(V, rules = "funder2019")
  expect_equal(V_[["Interpretation"]], "large", ignore_attr = TRUE)
  expect_s3_class(V_[["Interpretation"]], "effectsize_interpret")
  expect_output(print(V_), "large")
  expect_output(print(V_), "Interpretation rule: funder2019")

  expect_error(interpret(d), "must specify")

  d1 <- cohens_d(mtcars$wt, mu = 4)
  d2 <- cohens_d(-mtcars$wt, mu = -4)
  d1_ <- interpret(d1, rules = "cohen1988")
  d2_ <- interpret(d2, rules = "cohen1988")

  expect_identical(d1_$Interpretation, d2_$Interpretation)
  expect_identical(d1_[[1]], d1[[1]])
  expect_identical(d2_[[1]], d2[[1]])
})
