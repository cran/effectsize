#' Effect size for differences
#'
#' Compute effect size indices for standardized differences: Cohen's *d*,
#' Hedges' *g* and Glass’s *delta*. (This function returns the **population**
#' estimate.)
#' \cr\cr
#' Both Cohen's *d* and Hedges' *g* are the estimated the standardized
#' difference between the means of two populations. Hedges' *g* provides a bias
#' correction (using the exact method) to Cohen's *d* for small sample sizes.
#' For sample sizes > 20, the results for both statistics are roughly
#' equivalent. Glass’s *delta* is appropriate when the standard deviations are
#' significantly different between the populations, as it uses only the *second*
#' group's standard deviation.
#'
#' @param x A formula, a numeric vector, or a character name of one in `data`.
#' @param y A numeric vector, a grouping (character / factor) vector, a or a
#'   character  name of one in `data`. Ignored if `x` is a formula.
#' @param data An optional data frame containing the variables.
#' @param pooled_sd If `TRUE` (default), a [sd_pooled()] is used (assuming equal
#'   variance). Else the mean SD from both groups is used instead.
#' @param paired If `TRUE`, the values of `x` and `y` are considered as paired.
#'   This produces an effect size that is equivalent to the one-sample effect
#'   size on `x - y`.
#' @param iterations,correction deprecated.
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
#' @inheritSection effectsize-CIs Confidence Intervals
#' @inheritSection effectsize-CIs CI Contains Zero
#'
#' @return A data frame with the effect size ( `Cohens_d`, `Hedges_g`,
#'   `Glass_delta`) and their CIs (`CI_low` and `CI_high`).
#'
#' @seealso [d_to_common_language()] [sd_pooled()]
#' @family effect size indices
#'
#' @examples
#'
#' # two-sample tests -----------------------
#'
#' # using formula interface
#' cohens_d(mpg ~ am, data = mtcars)
#' cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
#' cohens_d(mpg ~ am, data = mtcars, mu = -5)
#' hedges_g(mpg ~ am, data = mtcars)
#' glass_delta(mpg ~ am, data = mtcars)
#' print(cohens_d(mpg ~ am, data = mtcars), append_CL = TRUE)
#'
#' # other acceptable ways to specify arguments
#' glass_delta(sleep$extra, sleep$group)
#' hedges_g("extra", "group", data = sleep)
#' cohens_d(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], paired = TRUE)
#' # cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1,
#' #          data = sleep, paired = TRUE)
#'
#' # one-sample tests -----------------------
#'
#' cohens_d("wt", data = mtcars, mu = 3)
#' hedges_g("wt", data = mtcars, mu = 3)
#'
#' # interpretation -----------------------
#'
#' interpret_d(0.4, rules = "cohen1988")
#' d_to_common_language(0.4)
#' interpret_g(0.4, rules = "sawilowsky2009")
#' interpret_delta(0.4, rules = "gignac2016")
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
#' with Welch's t-test. https://doi.org/10.31234/osf.io/tu6mp
#'
#' - Hedges, L. V. & Olkin, I. (1985). Statistical methods for
#' meta-analysis. Orlando, FL: Academic Press.
#'
#' - Hunter, J. E., & Schmidt, F. L. (2004). Methods of meta-analysis:
#' Correcting error and bias in research findings. Sage.
#'
#' @importFrom stats var model.frame
#' @export
cohens_d <- function(x,
                     y = NULL,
                     data = NULL,
                     pooled_sd = TRUE,
                     mu = 0,
                     paired = FALSE,
                     ci = 0.95,
                     verbose = TRUE,
                     ...) {
  if (inherits(x, "htest")) {
    if (!grepl("t-test", x$method)) {
      stop("'x' is not a t-test!", call. = FALSE)
    }
    return(effectsize(x, type = "d", ci = ci, verbose = verbose))
  } else if (inherits(x, "BFBayesFactor")) {
    if (!inherits(x@numerator[[1]], c("BFoneSample", "BFindepSample"))) {
      stop("'x' is not a t-test!", call. = FALSE)
    }
    return(effectsize(x, ci = ci, verbose = verbose))
  }


  .effect_size_difference(
    x,
    y = y,
    data = data,
    type = "d",
    pooled_sd = pooled_sd,
    mu = mu,
    paired = paired,
    ci = ci,
    verbose = verbose
  )
}

#' @rdname cohens_d
#' @export
hedges_g <- function(x,
                     y = NULL,
                     data = NULL,
                     pooled_sd = TRUE,
                     mu = 0,
                     paired = FALSE,
                     ci = 0.95,
                     verbose = TRUE,
                     ...,
                     correction) {
  if (!missing(correction)) {
    warning("`correction` argument is deprecated. *Exact* bias correction method is used.",
      call. = FALSE, immediate. = TRUE
    )
  }

  if (inherits(x, "htest")) {
    if (!grepl("t-test", x$method)) {
      stop("'x' is not a t-test!", call. = FALSE)
    }
    return(effectsize(x, type = "g", ci = ci, verbose = verbose))
  } else if (inherits(x, "BFBayesFactor")) {
    if (!inherits(x@numerator[[1]], c("BFoneSample", "BFindepSample"))) {
      stop("'x' is not a t-test!", call. = FALSE)
    }
    return(effectsize(x, ci = ci, verbose = verbose))
  }

  .effect_size_difference(
    x,
    y = y,
    data = data,
    type = "g",
    pooled_sd = pooled_sd,
    mu = mu,
    paired = paired,
    ci = ci,
    verbose = verbose
  )
}

#' @rdname cohens_d
#' @export
glass_delta <- function(x,
                        y = NULL,
                        data = NULL,
                        mu = 0,
                        ci = 0.95,
                        verbose = TRUE,
                        ...,
                        iterations) {
  if (!missing(iterations)) {
    warning("`iterations` argument is deprecated. Parametric CIs are estimated.",
      call. = FALSE, immediate. = TRUE
    )
  }

  .effect_size_difference(
    x,
    y = y,
    data = data,
    mu = mu,
    type = "delta",
    ci = ci,
    verbose = verbose
  )
}



#' @importFrom stats sd na.omit complete.cases
#' @keywords internal
.effect_size_difference <- function(x,
                                    y = NULL,
                                    data = NULL,
                                    type = "d",
                                    mu = 0,
                                    pooled_sd = TRUE,
                                    paired = FALSE,
                                    ci = 0.95,
                                    verbose = TRUE,
                                    ...) {
  out <- .deal_with_cohens_d_arguments(x, y, data, verbose)
  x <- out$x
  y <- out$y

  if (is.null(y)) {
    if (type == "delta") {
      stop("For Glass' Delta, please provide data from two samples.", call. = FALSE)
    }
    y <- rep(0, length.out = length(x))
    paired <- TRUE
  }

  # Compute index
  if (paired) {
    o <- stats::complete.cases(x, y)
    x <- x[o]
    y <- y[o]

    d <- mean(x - y)
    n <- length(x)
    s <- stats::sd(x - y)

    hn <- 1 / (n - 1)
    se <- s / sqrt(n)
    df <- n - 1

    pooled_sd <- NULL
  } else {
    x <- stats::na.omit(x)
    y <- stats::na.omit(y)

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
        df <- n - 2
      } else {
        s <- sqrt((s1^2 + s2^2) / 2)
        hn <- (2 * (n2 * s1^2 + n1 * s2^2)) / (n1 * n2 * (s1^2 + s2^2))
        se1 <- sqrt(s1^2 / n1)
        se2 <- sqrt(s2^2 / n2)
        se <- sqrt(se1^2 + se2^2)
        df <- se^4 / (se1^4 / (n1 - 1) + se2^4 / (n2 - 1))
      }
    } else if (type == "delta") {
      pooled_sd <- NULL

      s <- s2
      hn <- 1 / n2 + s1^2 / (n1 * s2^2)
      se <- (s2 * sqrt(1 / n2 + s1^2 / (n1 * s2^2)))
      df <- n2 - 1
    }
  }

  out <- data.frame(d = (d - mu) / s)
  types <- c("d" = "Cohens_d", "g" = "Hedges_g", "delta" = "Glass_delta")
  colnames(out) <- types[type]

  ci_method <- NULL
  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)

    # Add cis
    out$CI <- ci

    t <- (d - mu) / se
    ts <- .get_ncp_t(t, df, ci)

    out$CI_low <- ts[1] * sqrt(hn)
    out$CI_high <- ts[2] * sqrt(hn)
    ci_method <- list(method = "ncp", distribution = "t")
  }


  if (type == "g") {
    J <- exp(lgamma(df / 2) - log(sqrt(df / 2)) - lgamma((df - 1) / 2)) # exact method

    out[, colnames(out) %in% c("Hedges_g", "CI_low", "CI_high")] <-
      out[, colnames(out) %in% c("Hedges_g", "CI_low", "CI_high")] * J
  }

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  attr(out, "paired") <- paired
  attr(out, "correction") <- type == "g" # Don't actually need this
  attr(out, "pooled_sd") <- pooled_sd
  attr(out, "mu") <- mu
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  return(out)
}






# Utils -------------------------------------------------------------------



#' @keywords internal
#' @importFrom stats terms
#' @importFrom stats delete.response
.deal_with_cohens_d_arguments <- function(x, y = NULL, data = NULL, verbose = TRUE) {

  # Sanity checks
  if ((is.character(x) | is.character(y)) && is.null(data)) {
    stop("Please provide data argument.")
  }


  ## Preprocess data

  # Formula
  if (inherits(x, "formula")) {
    if (length(x) != 3) {
      stop("Formula must be two sided.", call. = FALSE)
    }

    mf <- stats::model.frame(formula = x, data = data)

    x <- mf[[1]]
    if (ncol(mf) == 1) {
      y <- NULL
    } else if (ncol(mf) == 2) {
      y <- mf[[2]]
    } else {
      stop("Formula must have only one term on the RHS.", call. = FALSE)
    }

    if (!is.null(y) && !is.factor(y)) y <- factor(y)
  }

  if (is.character(x)) {
    if (is.null(x <- data[[xn <- x]])) {
      stop("Column ", xn, " missing from data.", call. = FALSE)
    }
  }

  if (is.character(y)) {
    if (is.null(y <- data[[yn <- y]])) {
      stop("Column ", yn, " missing from data.", call. = FALSE)
    }
  }

  if (!is.numeric(x)) {
    stop("Cannot compute effect size for a non-numeric vector.", call. = FALSE)
  }

  # If y is a factor
  if (!is.null(y)) {
    if (!is.numeric(y)) {
      if (length(unique(y)) > 2) {
        stop("Cannot compute the difference as a factor with more than 2 levels has been provided.",
          call. = FALSE
        )
      }
      if (ifelse(inherits(x, "Pair"), nrow(x), length(x)) != length(y)) {
        stop("Grouping variable must be the same length.", call. = FALSE)
      }

      data <- split(x, y)
      data <- Filter(length, data)
      x <- data[[1]]
      y <- data[[2]]
    } else if (verbose && length(unique(y)) == 2) {
      warning(
        "'y' is numeric but has only 2 unique values. If this is a grouping variable, convert it to a factor.",
        call. = FALSE
      )
    }
  }

  if (inherits(x, "Pair")) {
    x <- -apply(x, 1, diff)
  }

  list(x = x, y = y)
}

# .delta_ci <- function(x, y, mu = 0, ci = 0.95, iterations = 200) {
#   boot_delta <- function(data, .i, mu = 0) {
#     .x <- sample(x, replace = TRUE)
#     .y <- sample(y, replace = TRUE)
#
#     d <- mean(.x) - mean(.y)
#     s <- stats::sd(.y)
#     (d - mu) / s
#   }
#
#   # dud, not actually used
#   data <- data.frame(
#     i = seq_along(c(x, y))
#   )
#
#   R <- boot::boot(
#     data = data,
#     statistic = boot_delta,
#     R = iterations,
#     mu = mu
#   )
#
#   out <- as.data.frame(
#     bayestestR::ci(na.omit(R$t), ci = ci, verbose = FALSE)
#   )
#   out$CI <- ci
#   out
# }
