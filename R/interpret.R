# Rules ---------------------------------------------------------------


#' Create an Interpretation Grid
#'
#' Create a container for interpretation rules of thumb. Usually used in conjunction with [interpret].
#'
#' @param values Vector of reference values (edges defining categories or
#'   critical values).
#' @param labels Labels associated with each category. If `NULL`, will try to
#'   infer it from `values` (if it is a named vector or a list), otherwise, will
#'   return the breakpoints.
#' @param name Name of the set of rules (will be printed).
#' @param right logical, for threshold-type rules, indicating if the thresholds
#'   themselves should be included in the interval to the right (lower values)
#'   or in the interval to the left (higher values).
#'
#'
#'
#' @seealso [interpret()]
#'
#' @examples
#' rules(c(0.05), c("significant", "not significant"), right = FALSE)
#' rules(c(0.2, 0.5, 0.8), c("small", "medium", "large"))
#' rules(c("small" = 0.2, "medium" = 0.5), name = "Cohen's Rules")
#' @export
rules <- function(values, labels = NULL, name = NULL, right = TRUE) {
  if (is.null(labels)) {
    if (is.list(values)) {
      values <- unlist(values)
    }
    if (is.null(names(values))) {
      labels <- values
    } else {
      labels <- names(values)
    }
  }

  # validation checks
  if (length(labels) < length(values)) {
    insight::format_error("There cannot be less labels than reference values!")
  } else if (length(labels) > length(values) + 1) {
    insight::format_error("Too many labels for the number of reference values!")
  }

  if (!is.numeric(values)) {
    insight::format_error("Reference values must be numeric.")
  }

  if (length(values) == length(labels) - 1) {
    if (is.unsorted(values)) {
      insight::format_error("Reference values must be sorted.")
    }
  } else {
    right <- NULL
  }

  # Store and return
  out <- list(
    values = values,
    labels = labels
  )

  if (is.null(name)) {
    attr(out, "rule_name") <- "Custom rules"
  } else {
    attr(out, "rule_name") <- name
  }

  attr(out, "right") <- right
  class(out) <- c("rules", "list")
  out
}




#' @rdname rules
#' @param x An arbitrary R object.
#' @export
is.rules <- function(x) inherits(x, "rules")




# Interpret ---------------------------------------------------------------



#' Generic Function for Interpretation
#'
#' Interpret a value based on a set of rules. See [rules()].
#'
#' @param x Vector of value break points (edges defining categories), or a data
#'   frame of class `effectsize_table`.
#' @param rules Set of [rules()]. When `x` is a data frame, can be a name of an
#'   established set of rules.
#' @param transform a function (or name of a function) to apply to `x` before
#'   interpreting. See examples.
#' @param ... Currently not used.
#' @inheritParams rules
#'
#' @return
#' - For numeric input: A character vector of interpretations.
#' - For data frames: the `x` input with an additional `Interpretation` column.
#'
#' @seealso [rules()]
#' @examples
#' rules_grid <- rules(c(0.01, 0.05), c("very significant", "significant", "not significant"))
#' interpret(0.001, rules_grid)
#' interpret(0.021, rules_grid)
#' interpret(0.08, rules_grid)
#' interpret(c(0.01, 0.005, 0.08), rules_grid)
#'
#' interpret(c(0.35, 0.15), c("small" = 0.2, "large" = 0.4), name = "Cohen's Rules")
#' interpret(c(0.35, 0.15), rules(c(0.2, 0.4), c("small", "medium", "large")))
#'
#' bigness <- rules(c(1, 10), c("small", "medium", "big"))
#' interpret(abs(-5), bigness)
#' interpret(-5, bigness, transform = abs)
#'
#' # ----------
#' d <- cohens_d(mpg ~ am, data = mtcars)
#' interpret(d, rules = "cohen1988")
#'
#' d <- glass_delta(mpg ~ am, data = mtcars)
#' interpret(d, rules = "gignac2016")
#'
#' interpret(d, rules = rules(1, c("tiny", "yeah okay")))
#'
#' m <- lm(formula = wt ~ am * cyl, data = mtcars)
#' eta2 <- eta_squared(m)
#' interpret(eta2, rules = "field2013")
#'
#' X <- chisq.test(mtcars$am, mtcars$cyl == 8)
#' interpret(oddsratio(X), rules = "cohen1988")
#' interpret(cramers_v(X), rules = "lovakov2021")
#' @export
interpret <- function(x, ...) {
  UseMethod("interpret")
}


#' @rdname interpret
#' @export
interpret.numeric <- function(x, rules, name = attr(rules, "rule_name"),
                              transform = NULL, ...) {
  # This is meant to circumvent https://github.com/easystats/report/issues/442
  if (is.character(transform)) {
    transform <- match.fun(transform)
  } else if (!is.function(transform)) {
    transform <- identity
  }

  x_tran <- transform(x)

  if (!inherits(rules, "rules")) {
    rules <- rules(rules)
  }

  if (is.null(name)) name <- "Custom rules"
  attr(rules, "rule_name") <- name

  if (length(x_tran) > 1) {
    out <- vapply(x_tran, .interpret, rules = rules, FUN.VALUE = character(1L))
    if (is.matrix(x_tran) || is.array(x_tran)) {
      out <- structure(out, dim = dim(x_tran), dimnames = dimnames(x_tran))
    }
  } else {
    out <- .interpret(x_tran, rules = rules)
  }

  names(out) <- names(x_tran)

  class(out) <- c("effectsize_interpret", class(out))
  attr(out, "rules") <- rules
  out
}

#' @rdname interpret
#' @export
interpret.effectsize_table <- function(x, rules, transform = NULL, ...) {
  if (missing(rules)) insight::format_error("You {.b must} specify the rules of interpretation!")

  # This is meant to circumvent https://github.com/easystats/report/issues/442
  if (is.character(transform)) {
    transform <- match.fun(transform)
  } else if (!is.function(transform)) {
    transform <- identity
  }

  es_name <- colnames(x)[is_effectsize_name(colnames(x))]
  value <- transform(x[[es_name]])

  x$Interpretation <- switch(es_name,
    ## std diff
    Cohens_d = ,
    Hedges_g = ,
    Glass_delta = ,
    Mahalanobis_D = interpret_cohens_d(value, rules = rules),

    ## xtab cor
    Cramers_v = ,
    Cramers_v_adjusted = ,
    phi = ,
    phi_adjusted = ,
    Pearsons_c = ,
    Cohens_w = ,
    Tschuprows_t = ,
    Tschuprows_t_adjusted = ,
    Fei = interpret_fei(value, rules = rules),

    ## xtab 2x2
    Cohens_h = interpret_cohens_d(value, rules = rules),
    Odds_ratio = interpret_oddsratio(value, rules = rules, log = FALSE),
    log_Odds_ratio = interpret_oddsratio(value, rules = rules, log = TRUE),
    # TODO:
    # Risk_ratio = ,
    # log_Risk_ratio = ,

    ## xtab dep
    Cohens_g = interpret_cohens_g(value, rules = rules),

    ## anova
    Eta2 = ,
    Eta2_partial = ,
    Eta2_generalized = ,
    r2_semipartial = ,
    Epsilon2 = ,
    Epsilon2_partial = ,
    Omega2 = ,
    Omega2_partial = interpret_omega_squared(value, rules = rules),
    Cohens_f = ,
    Cohens_f_partial = interpret_omega_squared(f_to_eta2(value), rules = rules),
    Cohens_f2 = ,
    Cohens_f2_partial = interpret_omega_squared(f2_to_eta2(value), rules = rules),

    ## Rank
    r_rank_biserial = interpret_r(value, rules = rules),
    VDs_A = interpret_r(value * 2 - 1, rules = rules),
    Kendalls_W = interpret_kendalls_w(value, rules = rules),
    rank_epsilon_squared = ,
    rank_eta_squared = interpret_omega_squared(value, rules = rules),

    # TODO: add cles as a transformation of d?

    ## other
    r = interpret_r(value, rules = rules),
    d = interpret_cohens_d(value, rules = rules)
  )

  attr(x, "rules") <- attr(x$Interpretation, "rules")
  x
}

#' @keywords internal
.interpret <- function(x, rules) {
  if (is.na(x)) {
    return(NA_character_)
  }

  if (length(rules$values) == length(rules$labels)) {
    index <- which.min(abs(x - rules$values))
  } else {
    if (isTRUE(attr(rules, "right"))) {
      check <- x <= rules$values
    } else {
      check <- x < rules$values
    }

    if (any(check)) {
      index <- min(which(check))
    } else {
      index <- length(rules$labels)
    }
  }
  rules$labels[index]
}
