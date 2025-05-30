#' Effect Size for Rank Based ANOVA
#'
#' Compute rank epsilon squared (\eqn{E^2_R}) or rank eta squared
#' (\eqn{\eta^2_H}) (to accompany [stats::kruskal.test()]), and Kendall's *W*
#' (to accompany [stats::friedman.test()]) effect sizes for non-parametric (rank
#' sum) one-way ANOVAs.
#'
#' @inheritParams rank_biserial
#' @param x Can be one of:
#'   - A numeric or ordered vector, or a character name of one in `data`.
#'   - A list of vectors (for `rank_eta/epsilon_squared()`).
#'   - A matrix of `blocks x groups` (for `kendalls_w()`) (or `groups x blocks`
#'   if `blocks_on_rows = FALSE`). See details for the `blocks` and `groups`
#'   terminology used here.
#'   - A formula in the form of:
#'       - `DV ~ groups` for `rank_eta/epsilon_squared()`.
#'       - `DV ~ groups | blocks` for `kendalls_w()` (See details for the
#'       `blocks` and `groups` terminology used here).
#' @param groups,blocks A factor vector giving the group / block for the
#'   corresponding elements of `x`, or a character name of one in `data`.
#'   Ignored if `x` is not a vector.
#' @param blocks_on_rows Are blocks on rows (`TRUE`) or columns (`FALSE`).
#' @param iterations The number of bootstrap replicates for computing confidence
#'   intervals. Only applies when `ci` is not `NULL`.
#'
#'
#' @details
#' The rank epsilon squared and rank eta squared are appropriate for
#' non-parametric tests of differences between 2 or more samples (a rank based
#' ANOVA). See [stats::kruskal.test]. Values range from 0 to 1, with larger
#' values indicating larger differences between groups.
#' \cr\cr
#' Kendall's *W* is appropriate for non-parametric tests of differences between
#' 2 or more dependent samples (a rank based rmANOVA), where each `group` (e.g.,
#' experimental condition) was measured for each `block` (e.g., subject). This
#' measure is also common as a measure of reliability of the rankings of the
#' `groups` between raters (`blocks`). See [stats::friedman.test]. Values range
#' from 0 to 1, with larger values indicating larger differences between groups
#' / higher agreement between raters.
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' Confidence intervals for \eqn{E^2_R}, \eqn{\eta^2_H}, and Kendall's *W* are
#' estimated using the bootstrap method (using the `{boot}` package).
#'
#' @inheritSection rank_biserial Ties
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection effectsize_CIs Bootstrapped CIs
#' @inheritSection print.effectsize_table Plotting with `see`
#'
#'
#' @return A data frame with the effect size and its CI.
#'
#' @family rank-based effect sizes
#' @family effect sizes for ANOVAs
#'
#' @examples
#' \donttest{
#' # Rank Eta/Epsilon Squared
#' # ========================
#'
#' rank_eta_squared(mpg ~ cyl, data = mtcars)
#'
#' rank_epsilon_squared(mpg ~ cyl, data = mtcars)
#'
#'
#'
#' # Kendall's W
#' # ===========
#' dat <- data.frame(
#'   cond = c("A", "B", "A", "B", "A", "B"),
#'   ID = c("L", "L", "M", "M", "H", "H"),
#'   y = c(44.56, 28.22, 24, 28.78, 24.56, 18.78)
#' )
#' (W <- kendalls_w(y ~ cond | ID, data = dat, verbose = FALSE))
#'
#' interpret_kendalls_w(0.11)
#' interpret(W, rules = "landis1977")
#' }
#'
#' @references
#' - Kendall, M.G. (1948) Rank correlation methods. London: Griffin.
#'
#' - Tomczak, M., & Tomczak, E. (2014). The need to report effect size estimates
#' revisited. An overview of some recommended measures of effect size. Trends in
#' sport sciences, 1(21), 19-25.
#'
#' @export
rank_epsilon_squared <- function(x, groups, data = NULL,
                                 ci = 0.95, alternative = "greater",
                                 iterations = 200,
                                 verbose = TRUE, ...) {
  alternative <- .match.alt(alternative, FALSE)

  if (.is_htest_of_type(x, "Kruskal-Wallis", "Kruskal-Wallis-test")) {
    return(effectsize(x, type = "epsilon", ci = ci, iterations = iterations, alternative = alternative))
  }

  ## pep data
  data <- .get_data_multi_group(x, groups, data,
    allow_ordered = TRUE,
    verbose = verbose, ...
  )

  ## compute
  out <- data.frame(rank_epsilon_squared = .repsilon(data))

  ## CI
  if (.test_ci(ci) && insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
    out <- cbind(out, .boot_two_group_es(
      data, .repsilon, iterations,
      ci, alternative
    ))
    ci_method <- list(method = "percentile bootstrap", iterations = iterations)
  } else {
    ci_method <- alternative <- ci <- NULL
  }

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}

#' @export
#' @rdname rank_epsilon_squared
rank_eta_squared <- function(x, groups, data = NULL,
                             ci = 0.95, alternative = "greater",
                             iterations = 200,
                             verbose = TRUE, ...) {
  alternative <- .match.alt(alternative, FALSE)

  if (.is_htest_of_type(x, "Kruskal-Wallis", "Kruskal-Wallis-test")) {
    return(effectsize(x, type = "eta", ci = ci, iterations = iterations, alternative = alternative))
  }

  ## pep data
  data <- .get_data_multi_group(x, groups, data,
    allow_ordered = TRUE,
    verbose = verbose, ...
  )

  out <- data.frame(rank_eta_squared = .reta(data))

  ## CI
  if (.test_ci(ci) && insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
    out <- cbind(out, .boot_two_group_es(
      data, .reta, iterations,
      ci, alternative
    ))
    ci_method <- list(method = "percentile bootstrap", iterations = iterations)
  } else {
    ci_method <- alternative <- ci <- NULL
  }

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}






#' @rdname rank_epsilon_squared
#' @export
kendalls_w <- function(x, groups, blocks, data = NULL,
                       blocks_on_rows = TRUE,
                       ci = 0.95, alternative = "greater",
                       iterations = 200,
                       verbose = TRUE, ...) {
  alternative <- .match.alt(alternative, FALSE)

  if (.is_htest_of_type(x, "Friedman", "Friedman-test")) {
    return(effectsize(x, ci = ci, iterations = iterations, verbose = verbose, alternative = alternative))
  }

  ## prep data
  if (is.matrix(x) && !blocks_on_rows) x <- t(x)
  data <- .get_data_nested_groups(x, groups, blocks, data,
    allow_ordered = TRUE,
    verbose = verbose, ...
  )
  data <- stats::na.omit(data) # wide data - drop non complete cases

  ## compute
  W <- .kendalls_w(data, verbose = verbose)
  out <- data.frame(Kendalls_W = W)

  ## CI
  if (.test_ci(ci) && insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
    out <- cbind(out, .kendalls_w_ci(data, ci, alternative, iterations))
    ci_method <- list(method = "percentile bootstrap", iterations = iterations)
  } else {
    ci_method <- alternative <- ci <- NULL
  }

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}

# Utils -------------------------------------------------------------------

## Get ----

#' @keywords internal
.repsilon <- function(data) {
  model <- suppressWarnings(stats::kruskal.test(data$x, data$groups))

  H <- unname(model$statistic)
  n <- nrow(data)

  E <- H / ((n^2 - 1) / (n + 1))
}

#' @keywords internal
.reta <- function(data) {
  model <- suppressWarnings(stats::kruskal.test(data$x, data$groups))

  k <- nlevels(data$groups)
  n <- nrow(data)
  E <- model$statistic

  pmax(0, (E - k + 1) / (n - k))
}


#' @keywords internal
.kendalls_w <- function(data, verbose) {
  rankings <- apply(data, 1, .safe_ranktransform, verbose = verbose)
  rankings <- t(rankings) # keep dims

  n <- ncol(rankings) # items
  m <- nrow(rankings) # judges
  R <- colSums(rankings)

  no_ties <- apply(rankings, 1, function(x) length(x) == insight::n_unique(x))
  if (all(no_ties)) {
    S <- stats::var(R) * (n - 1)
    W <- (12 * S) / (m^2 * (n^3 - n))
  } else {
    if (verbose) {
      insight::format_warning(
        sprintf(
          "%d block(s) contain ties%s.",
          sum(!no_ties),
          ifelse(any(apply(as.data.frame(rankings)[!no_ties, ], 1, insight::n_unique) == 1),
            ", some containing only 1 unique ranking", ""
          )
        )
      )
    }

    Tj <- sum(apply(rankings, 1, function(.r) {
      TTi <- table(.r)
      sum(TTi^3 - TTi)
    }))

    W <- (12 * sum(R^2) - 3 * (m^2) * n * ((n + 1)^2)) /
      (m^2 * (n^3 - n) - m * Tj)
  }
  W
}

## CI ----

#' @keywords internal
.boot_two_group_es <- function(data, foo_es, iterations,
                               ci, alternative, lim) {
  ci.level <- .adjust_ci(ci, alternative)

  boot_fun <- function(.data, .i) {
    split(.data$x, .data$groups) <-
      lapply(
        split(.data$x, .data$groups),
        function(v) {
          if (length(v) < 2L) {
            return(v)
          }
          sample(v, size = length(v), replace = TRUE)
        }
      )
    foo_es(.data)
  }

  R <- boot::boot(
    data = data,
    statistic = boot_fun,
    R = iterations
  )

  bCI <- boot::boot.ci(R, conf = ci.level, type = "perc")$percent
  bCI <- utils::tail(as.vector(bCI), 2)

  out <- data.frame(
    CI = ci,
    CI_low = bCI[1],
    CI_high = bCI[2]
  )
  .limit_ci(out, alternative, 0, 1)
}

#' @keywords internal
.kendalls_w_ci <- function(data, ci, alternative, iterations) {
  ci.level <- .adjust_ci(ci, alternative)

  boot_w <- function(.data, .i) {
    .kendalls_w(.data[.i, ], verbose = FALSE) # sample rows
  }

  R <- boot::boot(
    data = data,
    statistic = boot_w,
    R = iterations
  )

  bCI <- boot::boot.ci(R, conf = ci.level, type = "perc")$percent
  bCI <- utils::tail(as.vector(bCI), 2)

  out <- data.frame(
    CI = ci,
    CI_low = bCI[1],
    CI_high = bCI[2]
  )
  .limit_ci(out, alternative, 0, 1)
}
