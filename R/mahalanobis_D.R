#' Mahalanobis' *D* (a multivariate Cohen's *d*)
#'
#' Compute effect size indices for standardized difference between two normal
#' multivariate distributions or between one multivariate distribution and a
#' defined point. This is the standardized effect size for Hotelling's \eqn{T^2}
#' test (e.g., `DescTools::HotellingsT2Test()`). *D* is computed as:
#' \cr\cr
#' \deqn{D = \sqrt{(\bar{X}_1-\bar{X}_2-\mu)^T \Sigma_p^{-1} (\bar{X}_1-\bar{X}_2-\mu)}}
#' \cr\cr
#' Where \eqn{\bar{X}_i} are the column means, \eqn{\Sigma_p} is the *pooled*
#' covariance matrix, and \eqn{\mu} is a vector of the null differences for each
#' variable. When there is only one variate, this formula reduces to Cohen's
#' *d*.
#'
#' @inheritParams cohens_d
#' @param x,y A data frame or matrix. Any incomplete observations (with `NA`
#'   values) are dropped. `x` can also be a formula (see details) in which case
#'   `y` is ignored.
#' @param pooled_cov Should equal covariance be assumed? Currently only
#'   `pooled_cov = TRUE` is supported.
#' @param mu A named list/vector of the true difference in means for each
#'   variable. Can also be a vector of length 1, which will be recycled.
#' @param ... Not used.
#'
#' @inheritSection effectsize_CIs Confidence (Compatibility) Intervals (CIs)
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection print.effectsize_table Plotting with `see`
#'
#' @details
#' To specify a `x` as a formula:
#' - Two sample case: `DV1 + DV2 ~ group` or `cbind(DV1, DV2) ~ group`
#' - One sample case: `DV1 + DV2 ~ 1` or `cbind(DV1, DV2) ~ 1`
#'
#' @return A data frame with the `Mahalanobis_D` and potentially its CI
#'   (`CI_low` and `CI_high`).
#'
#' @seealso [stats::mahalanobis()], [cov_pooled()]
#' @family standardized differences
#'
#' @references
#' - Del Giudice, M. (2017). Heterogeneity coefficients for Mahalanobis' D as a multivariate effect size. Multivariate Behavioral Research, 52(2), 216-221.
#' - Mahalanobis, P. C. (1936). On the generalized distance in statistics. National Institute of Science of India.
#' - Reiser, B. (2001). Confidence intervals for the Mahalanobis distance. Communications in Statistics-Simulation and Computation, 30(1), 37-45.
#'
#' @examples
#' ## Two samples --------------
#' mtcars_am0 <- subset(mtcars, am == 0,
#'   select = c(mpg, hp, cyl)
#' )
#' mtcars_am1 <- subset(mtcars, am == 1,
#'   select = c(mpg, hp, cyl)
#' )
#'
#' mahalanobis_d(mtcars_am0, mtcars_am1)
#'
#' # Or
#' mahalanobis_d(mpg + hp + cyl ~ am, data = mtcars)
#'
#' mahalanobis_d(mpg + hp + cyl ~ am, data = mtcars, alternative = "two.sided")
#'
#' # Different mu:
#' mahalanobis_d(mpg + hp + cyl ~ am,
#'   data = mtcars,
#'   mu = c(mpg = -4, hp = 15, cyl = 0)
#' )
#'
#'
#' # D is a multivariate d, so when only 1 variate is provided:
#' mahalanobis_d(hp ~ am, data = mtcars)
#'
#' cohens_d(hp ~ am, data = mtcars)
#'
#'
#' # One sample ---------------------------
#' mahalanobis_d(mtcars[, c("mpg", "hp", "cyl")])
#'
#' # Or
#' mahalanobis_d(mpg + hp + cyl ~ 1,
#'   data = mtcars,
#'   mu = c(mpg = 15, hp = 5, cyl = 3)
#' )
#'
#' @export
mahalanobis_d <- function(x, y = NULL, data = NULL,
                          pooled_cov = TRUE, mu = 0,
                          ci = 0.95, alternative = "greater",
                          verbose = TRUE, ...) {
  # TODO add paired samples case DV1 + DV2 ~ 1 | ID
  alternative <- .match.alt(alternative, FALSE)
  data <- .get_data_multivariate(x, y, data, verbose = verbose, ...)
  x <- data[["x"]]
  y <- data[["y"]]


  # deal with mu
  if (is.vector(mu)) {
    if (length(mu) == 1L) {
      mu <- rep(mu, length.out = ncol(x))
      names(mu) <- colnames(x)
    } else if (length(mu) != ncol(x) || is.null(names(mu))) {
      insight::format_error("mu must be of length 1 or a named vector/list of length ncol(x).")
    }

    mu <- as.list(mu)
  }

  if (!is.list(mu)) {
    insight::format_error("mu must be of length 1 or a named vector/list of length ncol(x).")
  } else if (!all(names(mu) == colnames(x))) {
    insight::format_error("x,y must have the same variables (in the same order)")
  } else if (!all(lengths(mu) == 1L) || !all(vapply(mu, is.numeric, TRUE))) {
    insight::format_error("Each element of mu must be a numeric vector of length 1.")
  }
  mu <- unlist(mu)

  if (is.null(y)) {
    d <- colMeans(x) - mu

    COV <- stats::cov(x)

    n <- nrow(x)
    p <- ncol(x)

    hn <- n
    df <- n - p
    ff <- hn * (df / (p * (n - 1)))
  } else {
    d <- colMeans(x) - colMeans(y) - mu

    if (!pooled_cov) {
      insight::format_warning("Non-pooled cov not supported.")
    }
    COV <- cov_pooled(x, y, verbose = verbose)

    n1 <- nrow(x)
    n2 <- nrow(y)
    p <- ncol(x)

    hn <- (n1 * n2) / (n1 + n2)
    df <- (n1 + n2 - p - 1)
    ff <- hn * (df / (p * (n1 + n2 - 2)))
  }

  out <- data.frame(Mahalanobis_D = sqrt(t(d) %*% solve(COV) %*% d))

  if (.test_ci(ci)) {
    # Add cis
    out$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    f <- ff * out[[1]]^2

    fs <- .get_ncp_F(f, p, df, ci.level)

    out$CI_low <- sqrt(fs[1] / hn)
    out$CI_high <- sqrt(fs[2] / hn)
    ci_method <- list(method = "ncp", distribution = "F")
    out <- .limit_ci(out, alternative, 0, Inf)
  } else {
    ci_method <- alternative <- NULL
  }

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  .someattributes(out) <- .nlist(
    pooled_cov,
    mu = sqrt(sum(mu^2)),
    ci, ci_method, alternative,
    approximate = FALSE
  )
  return(out)
}
