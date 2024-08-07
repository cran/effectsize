# NCP -------------------------

#' @keywords internal
.get_ncp_F <- function(f, df, df_error, conf.level = 0.9) {
  if (!is.finite(f) || !is.finite(df) || !is.finite(df_error)) {
    return(c(NA, NA))
  }

  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)

  lambda <- f * df
  ncp <- suppressWarnings(stats::optim(
    par = 1.1 * rep(lambda, 2),
    fn = function(x) {
      quan <- stats::qf(p = probs, df, df_error, ncp = x)
      sum(abs(quan - f))
    },
    control = list(abstol = 1e-09)
  ))
  f_ncp <- sort(ncp$par)

  if (f <= stats::qf(probs[1], df, df_error)) {
    f_ncp[2] <- 0
  }

  if (f <= stats::qf(probs[2], df, df_error)) {
    f_ncp[1] <- 0
  }

  return(f_ncp)
}

#' @keywords internal
.get_ncp_t <- function(t, df_error, conf.level = 0.95) {
  if (!is.finite(t) || !is.finite(df_error)) {
    return(c(NA, NA))
  }

  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)

  ncp <- suppressWarnings(stats::optim(
    par = 1.1 * rep(t, 2),
    fn = function(x) {
      quan <- stats::qt(p = probs, df = df_error, ncp = x)
      sum(abs(quan - t))
    },
    control = list(abstol = 1e-09)
  ))

  t_ncp <- unname(sort(ncp$par))

  return(t_ncp)
}

#' @keywords internals
.get_ncp_chi <- function(chisq, df, conf.level = 0.95) {
  if (!is.finite(chisq) || !is.finite(df)) {
    return(c(NA, NA))
  }

  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)

  ncp <- suppressWarnings(stats::optim(
    par = 1.1 * rep(chisq, 2),
    fn = function(x) {
      quan <- stats::qchisq(p = probs, df, ncp = x)
      sum(abs(quan - chisq))
    },
    control = list(abstol = 1e-09)
  ))
  chi_ncp <- sort(ncp$par)

  if (chisq <= stats::qchisq(probs[1], df)) {
    chi_ncp[2] <- 0
  }

  if (chisq <= stats::qchisq(probs[2], df)) {
    chi_ncp[1] <- 0
  }

  chi_ncp
}

# Validators --------------------------------------


#' @keywords internal
.test_ci <- function(ci) {
  if (is.null(ci)) {
    return(FALSE)
  }
  if (!is.numeric(ci) ||
    length(ci) != 1L ||
    ci < 0 ||
    ci > 1) {
    insight::format_error("ci must be a single numeric value between (0, 1)")
  }
  TRUE
}

#' @keywords internal
.adjust_ci <- function(ci, alternative) {
  if (alternative == "two.sided") {
    return(ci)
  }

  2 * ci - 1
}

#' @keywords internal
.limit_ci <- function(out, alternative, lb, ub) {
  if (alternative == "two.sided") {
    return(out)
  }

  if (alternative == "less") {
    out$CI_low <- lb
  } else if (alternative == "greater") {
    out$CI_high <- ub
  }

  out
}

#' @keywords internal
.match.alt <- function(alternative, two.sided = TRUE) {
  if (is.null(alternative)) {
    if (two.sided) {
      return("two.sided")
    } else {
      return("greater")
    }
  }

  match.arg(alternative, c("two.sided", "less", "greater"))
}
