#' Cohen's *d* and Other Standardized Differences
#'
#' Compute effect size indices for standardized mean differences: Cohen's *d*,
#' Hedges' *g* and Glass’s *delta* (\eqn{\Delta}). (This function returns the
#' **population** estimate.) Pair with any reported [`stats::t.test()`].
#' \cr\cr
#' Both Cohen's *d* and Hedges' *g* are the estimated the standardized
#' difference between the means of two populations. Hedges' *g* provides a
#' correction for small-sample bias (using the exact method) to Cohen's *d*. For
#' sample sizes > 20, the results for both statistics are roughly equivalent.
#' Glass’s *delta* is appropriate when the standard deviations are significantly
#' different between the populations, as it uses only the reference group's
#' standard deviation.
#'
#' @param x,y A numeric vector, or a character name of one in `data`.
#'   Any missing values (`NA`s) are dropped from the resulting vector.
#'   `x` can also be a formula (see [`stats::t.test()`]), in which case `y` is
#'   ignored.
#' @param alternative a character string specifying the alternative hypothesis;
#'   Controls the type of CI returned: `"two.sided"` (default, two-sided CI),
#'   `"greater"` or `"less"` (one-sided CI). Partial matching is allowed (e.g.,
#'   `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in [effectsize_CIs].
#' @param data An optional data frame containing the variables.
#' @param pooled_sd If `TRUE` (default), a [sd_pooled()] is used (assuming equal
#'   variance). Else the mean SD from both groups is used instead.
#' @param paired If `TRUE`, the values of `x` and `y` are considered as paired.
#'   This produces an effect size that is equivalent to the one-sample effect
#'   size on `x - y`. See also [repeated_measures_d()] for more options.
#' @param adjust Should the effect size be adjusted for small-sample bias using
#'   Hedges' method? Note that `hedges_g()` is an alias for
#'   `cohens_d(adjust = TRUE)`.
#' @param reference (Optional) character value of the "group" used as the
#'   reference. By default, the _second_ group is the reference group.
#' @param ... Arguments passed to or from other methods. When `x` is a formula,
#'   these can be `subset` and `na.action`.
#' @inheritParams chisq_to_phi
#' @inheritParams eta_squared
#' @inheritParams stats::t.test
#'
#' @note The indices here give the population estimated standardized difference.
#'   Some statistical packages give the sample estimate instead (without
#'   applying Bessel's correction).
#'
#' @details
#' Set `pooled_sd = FALSE` for effect sizes that are to accompany a Welch's
#' *t*-test (Delacre et al, 2021).
#'
#' @inheritSection effectsize_CIs Confidence (Compatibility) Intervals (CIs)
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection print.effectsize_table Plotting with `see`
#'
#' @return A data frame with the effect size ( `Cohens_d`, `Hedges_g`,
#'   `Glass_delta`) and their CIs (`CI_low` and `CI_high`).
#'
#' @family standardized differences
#' @seealso [rm_d()], [sd_pooled()], [t_to_d()], [r_to_d()]
#'
#' @examples
#' \donttest{
#' data(mtcars)
#' mtcars$am <- factor(mtcars$am)
#'
#' # Two Independent Samples ----------
#'
#' (d <- cohens_d(mpg ~ am, data = mtcars))
#' # Same as:
#' # cohens_d("mpg", "am", data = mtcars)
#' # cohens_d(mtcars$mpg[mtcars$am=="0"], mtcars$mpg[mtcars$am=="1"])
#'
#' # More options:
#' cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
#' cohens_d(mpg ~ am, data = mtcars, mu = -5)
#' cohens_d(mpg ~ am, data = mtcars, alternative = "less")
#' hedges_g(mpg ~ am, data = mtcars)
#' glass_delta(mpg ~ am, data = mtcars)
#'
#'
#' # One Sample ----------
#'
#' cohens_d(wt ~ 1, data = mtcars)
#'
#' # same as:
#' # cohens_d("wt", data = mtcars)
#' # cohens_d(mtcars$wt)
#'
#' # More options:
#' cohens_d(wt ~ 1, data = mtcars, mu = 3)
#' hedges_g(wt ~ 1, data = mtcars, mu = 3)
#'
#'
#' # Paired Samples ----------
#'
#' data(sleep)
#'
#' cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep)
#'
#' # same as:
#' # cohens_d(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], paired = TRUE)
#' # cohens_d(sleep$extra[sleep$group == 1] - sleep$extra[sleep$group == 2])
#' # rm_d(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], method = "z", adjust = FALSE)
#'
#' # More options:
#' cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, mu = -1, verbose = FALSE)
#' hedges_g(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, verbose = FALSE)
#'
#'
#' # Interpretation -----------------------
#' interpret_cohens_d(-1.48, rules = "cohen1988")
#' interpret_hedges_g(-1.48, rules = "sawilowsky2009")
#' interpret_glass_delta(-1.48, rules = "gignac2016")
#' # Or:
#' interpret(d, rules = "sawilowsky2009")
#'
#' # Common Language Effect Sizes
#' d_to_u3(1.48)
#' # Or:
#' print(d, append_CLES = TRUE)
#' }
#'
#' @references
#' - Algina, J., Keselman, H. J., & Penfield, R. D. (2006). Confidence intervals
#' for an effect size when variances are not equal. Journal of Modern Applied
#' Statistical Methods, 5(1), 2.
#'
#' - Cohen, J. (1988). Statistical power analysis for the behavioral
#' sciences (2nd Ed.). New York: Routledge.
#'
#' - Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021, May 7). Why
#' Hedges’ g*s based on the non-pooled standard deviation should be reported
#' with Welch's t-test. \doi{10.31234/osf.io/tu6mp}
#'
#' - Hedges, L. V. & Olkin, I. (1985). Statistical methods for
#' meta-analysis. Orlando, FL: Academic Press.
#'
#' - Hunter, J. E., & Schmidt, F. L. (2004). Methods of meta-analysis:
#' Correcting error and bias in research findings. Sage.
#'
#' @export
cohens_d <- function(x, y = NULL, data = NULL,
                     pooled_sd = TRUE, mu = 0, paired = FALSE,
                     reference = NULL,
                     adjust = FALSE,
                     ci = 0.95, alternative = "two.sided",
                     verbose = TRUE, ...) {
  var.equal <- eval.parent(match.call()[["var.equal"]])
  if (!is.null(var.equal)) pooled_sd <- var.equal

  .effect_size_difference(
    x,
    y = y, data = data,
    type = "d", adjust = adjust,
    pooled_sd = pooled_sd, mu = mu, paired = paired,
    reference = reference,
    ci = ci, alternative = alternative,
    verbose = verbose,
    ...
  )
}

#' @rdname cohens_d
#' @export
hedges_g <- function(x, y = NULL, data = NULL,
                     pooled_sd = TRUE, mu = 0, paired = FALSE,
                     reference = NULL,
                     ci = 0.95, alternative = "two.sided",
                     verbose = TRUE, ...) {
  cl <- match.call()
  cl[[1]] <- quote(effectsize::cohens_d)
  cl$adjust <- TRUE
  eval.parent(cl)
}

#' @rdname cohens_d
#' @export
glass_delta <- function(x, y = NULL, data = NULL,
                        mu = 0, adjust = TRUE,
                        reference = NULL,
                        ci = 0.95, alternative = "two.sided",
                        verbose = TRUE, ...) {
  .effect_size_difference(
    x,
    y = y, data = data,
    type = "delta",
    mu = mu, adjust = adjust,
    reference = reference,
    ci = ci, alternative = alternative,
    verbose = verbose,
    pooled_sd = NULL, paired = FALSE,
    ...
  )
}



#' @keywords internal
.effect_size_difference <- function(x, y = NULL, data = NULL,
                                    type = "d", adjust = FALSE,
                                    mu = 0, pooled_sd = TRUE, paired = FALSE,
                                    reference = NULL,
                                    ci = 0.95, alternative = "two.sided",
                                    verbose = TRUE, ...) {
  if (type == "d" && adjust) type <- "g"

  # TODO: Check if we can do anything with `reference` for these classes
  if (type != "delta") {
    if (.is_htest_of_type(x, "t-test")) {
      return(effectsize(x, type = type, verbose = verbose, data = data, ...))
    } else if (.is_BF_of_type(x, c("BFoneSample", "BFindepSample"), "t-squared")) {
      return(effectsize(x, ci = ci, verbose = verbose, ...))
    }
  }


  alternative <- .match.alt(alternative)
  out <- .get_data_2_samples(x, y, data, paired = paired, reference = reference, verbose = verbose, ...)
  x <- out[["x"]]
  y <- out[["y"]]
  paired <- out[["paired"]]

  if (verbose && paired && !is.null(y)) {
    insight::format_alert(
      "For paired samples, 'repeated_measures_d()' provides more options."
    )
  }

  if (is.null(y)) {
    if (type == "delta") {
      insight::format_error("For Glass' Delta, please provide data from two samples.")
    }
    y <- 0
    is_paired_or_onesample <- TRUE
  } else {
    is_paired_or_onesample <- paired
  }

  # Compute index
  if (is_paired_or_onesample) {
    if (type == "delta") {
      insight::format_error("This effect size is only applicable for two independent samples.")
    }

    d <- mean(x - y)
    n <- length(x)
    s <- stats::sd(x - y)

    hn <- 1 / n
    se <- s / sqrt(n)
    df1 <- n - 1

    pooled_sd <- NULL
  } else {
    d <- mean(x) - mean(y)

    s1 <- stats::sd(x)
    s2 <- stats::sd(y)

    n1 <- length(x)
    n2 <- length(y)
    n <- n1 + n2

    if (type %in% c("d", "g")) {
      if (pooled_sd) {
        s <- suppressWarnings(sd_pooled(x, y))
        hn <- (1 / n1 + 1 / n2)
        se <- s * sqrt(1 / n1 + 1 / n2)
        df1 <- n - 2
      } else {
        s <- sqrt((s1^2 + s2^2) / 2)
        hn <- (2 * (n2 * s1^2 + n1 * s2^2)) / (n1 * n2 * (s1^2 + s2^2))
        se1 <- sqrt(s1^2 / n1)
        se2 <- sqrt(s2^2 / n2)
        se <- sqrt(se1^2 + se2^2)
        df1 <- se^4 / (se1^4 / (n1 - 1) + se2^4 / (n2 - 1))
      }
    } else if (type == "delta") {
      pooled_sd <- NULL

      s <- s2
      hn <- 1 / n2 + s1^2 / (n1 * s2^2)
      se <- (s2 * sqrt(1 / n2 + s1^2 / (n1 * s2^2)))
      df1 <- n2 - 1
    }
  }

  out <- data.frame(d = (d - mu) / s)
  types <- c(d = "Cohens_d", g = "Hedges_g", delta = "Glass_delta")
  colnames(out) <- types[type]

  if (.test_ci(ci)) {
    # Add cis
    out$CI <- ci
    ci_level <- .adjust_ci(ci, alternative)

    t1 <- (d - mu) / se
    ts1 <- .get_ncp_t(t1, df1, ci_level)

    out$CI_low <- ts1[1] * sqrt(hn)
    out$CI_high <- ts1[2] * sqrt(hn)
    ci_method <- list(method = "ncp", distribution = "t")
    out <- .limit_ci(out, alternative, -Inf, Inf)
  } else {
    ci_method <- alternative <- NULL
  }

  if (adjust) {
    J <- .J(df1)
    col_to_adjust <- intersect(colnames(out), c(types[type], "CI_low", "CI_high"))
    out[, col_to_adjust] <- out[, col_to_adjust] * J

    if (type == "delta") {
      colnames(out)[1] <- "Glass_delta_adjusted"
    }
  }

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  .someattributes(out) <- .nlist(
    paired, pooled_sd, mu, ci, ci_method, alternative, adjust,
    approximate = FALSE
  )
  out
}

#' @keywords internal
.J <- function(df1) {
  exp(lgamma(df1 / 2) - log(sqrt(df1 / 2)) - lgamma((df1 - 1) / 2)) # exact method
}
