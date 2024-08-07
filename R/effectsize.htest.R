#' @export
#' @rdname effectsize
effectsize.htest <- function(model, type = NULL, verbose = TRUE, ...) {
  if (grepl("t-test", model$method, fixed = TRUE)) {
    .effectsize_t.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Pearson's Chi-squared", model$method, fixed = TRUE)) {
    .effectsize_chisq.test_dep(model, type = type, verbose = verbose, ...)
  } else if (grepl("Chi-squared test for given probabilities", model$method, fixed = TRUE)) {
    .effectsize_chisq.test_gof(model, type = type, verbose = verbose, ...)
  } else if (grepl("Fisher's Exact", model$method, fixed = TRUE)) {
    .effectsize_fisher.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("One-way", model$method, fixed = TRUE)) {
    .effectsize_oneway.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("McNemar", model$method, fixed = TRUE)) {
    .effectsize_mcnemar.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Wilcoxon", model$method, fixed = TRUE)) {
    .effectsize_wilcox.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Kruskal-Wallis", model$method, fixed = TRUE)) {
    .effectsize_kruskal.test(model, type = type, verbose = verbose, ...)
  } else if (grepl("Friedman", model$method, fixed = TRUE)) {
    .effectsize_friedman.test(model, type = type, verbose = verbose, ...)
  } else {
    if (verbose) {
      insight::format_warning(
        "This 'htest' method is not (yet?) supported.",
        "Returning 'parameters::model_parameters(model)'."
      )
    }
    parameters::model_parameters(model, verbose = verbose, ...)
  }
}

#' @keywords internal
.data_from_formula <- function(model_data, model, verbose = TRUE, ...) {
  if (is.null(model_data) && "data" %in% names(match.call())) {
    vars <- insight::get_parameters(model)$Parameter
    vars_split <- unlist(strsplit(vars, " by | and "))
    data_ellipsis <- eval.parent(match.call()[["data"]])
    if (!grepl("\\$|\\[", vars) && length(vars_split) > 1) {
      if (grepl("by|and", vars)) {
        vars <- sub("by|and", "~", vars, perl = TRUE)
        vars <- sub("and", "|", vars, fixed = TRUE)
        if (!grepl("|", vars, fixed = TRUE)) {
          form <- stats::as.formula(vars)
          data_out <- .resolve_formula(form, ...)
          data_out[[2]] <- factor(data_out[[2]])
        } else if (all(vars_split %in% names(data_ellipsis))) {
          # We need a special case for the Friedman test
          # because "In Ops.factor(w, t) : ‘|’ not meaningful for factors"
          # When used with the | operator within the formula
          data_out <- stats::model.frame(...)
          if (all(vars_split %in% names(data_out))) {
            data_out <- data_out[vars_split]
          } else {
            data_out <- NULL
          }
        }
      }
    } else if (grepl("$", vars, fixed = TRUE)) {
      # Special case for square bracket subsetting
      # E.g., x = dat$mpg[dat$am == 1], y = dat$mpg[dat$am == 0]
      vars_cols <- gsub("(\\b\\w+\\$)", paste0(match.call()[["data"]], "$"), vars)
      columns <- unlist(strsplit(vars_cols, " and ", fixed = TRUE))
      x <- eval(parse(text = columns[1]))
      y <- eval(parse(text = columns[2]))
      data_out <- list(x, y)
      # Not necessary to subset/na.omit here because not formula interface
    } else if (grepl("\\$|\\[", vars)) {
      # Special case for single-sample square bracket subsetting
      # E.g., x = mtcars[[col_y]]
      if (length(vars_split) == 1) {
        data_out <- data_ellipsis
        # Not necessary to subset/na.omit here because not formula interface
      } else {
        obj <- gsub(".*?\\[([^\\[\\]]+)\\].*", "\\1", vars, perl = TRUE)
        message("Is object '", obj, "' still available in your workspace?")
      }
    } else if (length(vars_split) == 1) {
      form <- stats::as.formula(paste0(vars, "~1"))
      data_out <- .resolve_formula(form, ...)
    } else if (verbose) {
      message("To use the `data` argument, consider using modifiers outside the formula.")
    }
  } else {
    data_out <- model_data
  }
  data_out
}

#' @keywords internal
.effectsize_t.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  model_data <- insight::get_data(model)
  data1 <- .data_from_formula(model_data, model, verbose, ...)

  approx1 <- is.null(data1)

  if (is.null(type) || tolower(type) == "cohens_d") type <- "d"
  if (tolower(type) == "hedges_g") type <- "g"

  cl <- match.call()
  cl <- cl[-which(names(cl) == "subset")]
  dots <- insight::compact_list(list(eval(cl, parent.frame())))

  dots$alternative <- model$alternative
  dots$ci <- attr(model$conf.int, "conf.level")
  dots$mu <- model$null.value
  dots$paired <- grepl("Paired", model$method, fixed = TRUE)
  dots$verbose <- verbose

  if (!type %in% c("d", "g")) {
    .fail_if_approx(approx1, if (startsWith(type, "rm")) "rm_d" else "cles")
  }

  if (approx1) {
    if (verbose) {
      insight::format_warning(
        "Unable to retrieve data from htest object.",
        "Returning an {.b approximate} effect size using t_to_d()."
      )
    }

    f <- t_to_d
    args1 <- list(
      t = unname(model$statistic),
      df_error = unname(model$parameter)
    )
  } else {
    if (inherits(data1, "data.frame") && ncol(data1) == 2) {
      data1[[2]] <- factor(data1[[2]])
    }
    data1 <- stats::na.omit(data1)

    if (inherits(data1, "numeric")) {
      args1 <- list(
        x = data1,
        pooled_sd = !grepl("Welch", model$method, fixed = TRUE)
      )
    } else {
      args1 <- list(
        x = data1[[1]],
        y = if (length(data1) == 2) data1[[2]],
        pooled_sd = !grepl("Welch", model$method, fixed = TRUE)
      )
    }

    if (type %in% c("d", "g")) {
      f <- switch(tolower(type),
        d = cohens_d,
        g = hedges_g
      )
    } else if (dots$paired && startsWith(type, "rm")) {
      args1[c("x", "y")] <- split(args1$x, args1$y)
      dots$paired <- args1$pooled_sd <- NULL
      args1$method <- gsub("^rm\\_", "", type)
      f <- rm_d
    } else {
      if (!dots$paired && !args1$pooled_sd) {
        insight::format_error("Common language effect size only applicable to Cohen's d with pooled SD.")
      }

      f <- switch(tolower(type),
        u1 = cohens_u1,
        u2 = cohens_u2,
        u3 = cohens_u3,
        vda = ,
        p_superiority = p_superiority,
        overlap = p_overlap
      )
    }
  }

  out <- do.call(f, c(args1, dots))
  attr(out, "approximate") <- approx1
  out
}

#' @keywords internal
.effectsize_chisq.test_dep <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  model_data <- insight::get_data(model)
  data1 <- .data_from_formula(model_data, model, verbose, ...)
  dots <- list(...)

  approx1 <- is.null(data1)

  Obs <- model$observed
  Exp <- model$expected

  if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
    insight::format_error("Cannot have empty rows/columns in the contingency tables.")
  }
  nr <- nrow(Obs)
  nc <- ncol(Obs)

  if (is.null(type)) type <- "cramers_v"

  if (grepl("(c|v|t|w|phi)$", tolower(type)) && tolower(type) != "nnt") {
    f <- switch(tolower(type),
      v = ,
      cramers_v = chisq_to_cramers_v,
      t = ,
      tschuprows_t = chisq_to_tschuprows_t,
      w = ,
      cohens_w = chisq_to_cohens_w,
      phi = chisq_to_phi,
      c = ,
      pearsons_c = chisq_to_pearsons_c
    )

    out <- f(
      chisq = .chisq(Obs, Exp),
      n = sum(Obs),
      nrow = nr,
      ncol = nc,
      verbose = verbose,
      ...
    )
  } else {
    f <- switch(tolower(type),
      or = ,
      oddsratio = oddsratio,
      rr = ,
      riskratio = riskratio,
      h = ,
      cohens_h = cohens_h,
      arr = arr,
      nnt = nnt
    )

    out <- f(x = model$observed, ...)
  }

  attr(out, "approximate") <- FALSE
  out
}

#' @keywords internal
.effectsize_fisher.test <- function(model, type = NULL, verbose = TRUE, ...) {
  if (is.null(type)) type <- "cramers_v"

  # If OR - return OR
  if (tolower(type) %in% c("or", "oddsratio")) {
    out <- data.frame(Odds_ratio = unname(model[["estimate"]]))
    ci_method <- NULL

    ci <- model[["conf.int"]]
    if (!is.null(ci)) {
      out$CI <- attr(ci, "conf.level")
      out$CI_low <- ci[1]
      out$CI_high <- ci[2]
      ci_method <- list("normal")
    }

    class(out) <- c("effectsize_table", "see_effectsize_table", "data.frame")
    .someattributes(out) <-
      .nlist(
        ci = out$CI, ci_method,
        approximate = FALSE,
        alternative = model[["alternative"]]
      )
    return(out)
  }

  dots <- list(...)
  if (!is.null(model[["conf.int"]])) dots$ci <- attr(model[["conf.int"]], "conf.level")
  if (!is.null(model[["alternative"]])) dots$alternative <- model[["alternative"]]

  data1 <- insight::get_data(model)
  .fail_if_approx(is.null(data1), type)

  f <- switch(tolower(type),
    v = ,
    cramers_v = cramers_v,
    t = ,
    tschuprows_t = tschuprows_t,
    w = ,
    cohens_w = cohens_w,
    phi = phi,
    c = ,
    pearsons_c = pearsons_c,
    or = ,
    oddsratio = oddsratio,
    rr = ,
    riskratio = riskratio,
    h = ,
    cohens_h = cohens_h,
    arr = arr,
    nnt = nnt
  )

  if (is.table(data1)) {
    args1 <- list(x = data1)
  } else {
    args1 <- list(x = data1[[1]], y = data1[[2]])
  }

  do.call(f, c(args1, dots))
}

#' @keywords internal
.effectsize_chisq.test_gof <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  model_data <- insight::get_data(model)
  data1 <- .data_from_formula(model_data, model, verbose, ...)
  dots <- list(...)

  approx1 <- is.null(data1)

  Obs <- model$observed
  Exp <- model$expected
  nr <- length(Obs)
  p <- Exp

  if (is.null(type)) type <- "fei"

  f <- switch(tolower(type),
    w = ,
    cohens_w = chisq_to_cohens_w,
    c = ,
    pearsons_c = chisq_to_pearsons_c,
    fei = chisq_to_fei,
    insight::format_error("The selected effect size is not supported for goodness-of-fit tests.")
  )

  out <- f(
    chisq = .chisq(Obs, Exp),
    n = sum(Obs),
    nrow = nr,
    ncol = 1,
    p = p,
    verbose = verbose,
    ...
  )

  attr(out, "approximate") <- FALSE
  out
}

#' @keywords internal
.effectsize_oneway.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  model_data <- insight::get_data(model)
  data1 <- .data_from_formula(model_data, model, verbose, ...)

  approx1 <- is.null(data1)

  approx1 <- grepl("not assuming", model$method, fixed = TRUE)
  if (approx1 && verbose) {
    insight::format_alert("`var.equal = FALSE` - effect size is an {.b approximation.}")
  }

  if (is.null(type)) type <- "eta"

  f <- switch(tolower(type),
    eta = ,
    eta2 = ,
    eta_squared = F_to_eta2,
    epsilon = ,
    epsilon2 = ,
    epsilon_squared = F_to_epsilon2,
    omega = ,
    omega2 = ,
    omega_squared = F_to_omega2,
    f = ,
    cohens_f = F_to_f,
    f2 = ,
    f_squared = ,
    cohens_f2 = F_to_f2
  )

  out <- f(
    f = model$statistic,
    df = model$parameter[1],
    df_error = model$parameter[2],
    verbose = verbose,
    ...
  )
  colnames(out)[1] <- sub("_partial", "", colnames(out)[1], fixed = TRUE)
  attr(out, "approximate") <- approx1
  out
}

#' @keywords internal
.effectsize_mcnemar.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  model_data <- insight::get_data(model)
  data1 <- .data_from_formula(model_data, model, verbose, ...)

  approx1 <- is.null(data1)

  .fail_if_approx(approx1, "cohens_g")

  if (inherits(data1, "table")) {
    out <- cohens_g(data1, verbose = verbose, ...)
  } else {
    out <- cohens_g(data1[[1]], data1[[2]], verbose = verbose, ...)
  }
  out
}

#' @keywords internal
.effectsize_wilcox.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  model_data <- insight::get_data(model)
  data1 <- .data_from_formula(model_data, model, verbose, ...)

  approx1 <- is.null(data1)

  if (is.null(type) || tolower(type) == "rank_biserial") type <- "rb"

  cl <- match.call()
  cl <- cl[-which(names(cl) == "subset")]
  dots <- list(eval(cl, parent.frame()))

  dots$alternative <- model$alternative
  dots$ci <- attr(model$conf.int, "conf.level")
  dots$mu <- model$null.value
  dots$paired <- grepl("signed rank", model$method, fixed = TRUE)

  .fail_if_approx(approx1, type)

  if (ncol(data1) == 2) {
    data1[[2]] <- factor(data1[[2]])
  }
  data1 <- stats::na.omit(data1)

  f <- switch(tolower(type),
    rb = rank_biserial,
    u1 = cohens_u1,
    u2 = cohens_u2,
    u3 = cohens_u3,
    overlap = p_overlap,
    vda = ,
    p_superiority = p_superiority,
    wmw_odds = wmw_odds
  )

  args1 <- list(
    x = data1[[1]],
    y = if (ncol(data1) == 2) data1[[2]],
    verbose = verbose
  )

  if (tolower(type) != "rb") {
    if (dots$paired) {
      insight::format_error("Common language effect size only applicable to 2-sample rank-biserial correlation.")
    }
    args1$parametric <- FALSE
  }

  out <- do.call(f, c(args1, dots))
  out
}

#' @keywords internal
.effectsize_kruskal.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  model_data <- insight::get_data(model)
  data1 <- .data_from_formula(model_data, model, verbose, ...)

  approx1 <- is.null(data1)

  if (is.null(type)) type <- "epsilon"

  .fail_if_approx(approx1, "rank_epsilon_squared")

  f <- switch(type,
    epsilon = rank_epsilon_squared,
    eta = rank_eta_squared
  )

  if (inherits(data1, "data.frame")) {
    out <- f(data1[[1]], data1[[2]], verbose = verbose, ...)
  } else {
    # data frame
    out <- f(data1, verbose = verbose, ...)
  }
  out
}

#' @keywords internal
.effectsize_friedman.test <- function(model, type = NULL, verbose = TRUE, ...) {
  # Get data?
  model_data <- insight::get_data(model)
  data1 <- .data_from_formula(model_data, model, verbose, ...)

  approx1 <- is.null(data1)

  .fail_if_approx(approx1, "kendalls_w")

  if (inherits(data1, "table")) {
    data1 <- as.data.frame(data1)[c("Freq", "Var2", "Var1")]
  }

  out <- kendalls_w(data1[[1]], data1[[2]], data1[[3]], verbose = verbose, ...)
  out
}

# Utils -------------------------------------------------------------------


#' @keywords internal
.chisq <- function(Obs, Exp) {
  sum(((Obs - Exp)^2) / Exp)
}

#' @keywords internal
.fail_if_approx <- function(approx, esf_name) {
  if (approx) {
    insight::format_error(
      "Unable to retrieve data from htest object.",
      sprintf("Try using '%s()' directly.", esf_name)
    )
  }
  invisible(NULL)
}
