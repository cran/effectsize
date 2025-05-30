# Specific tables ---------------------------------------------------------

#' @keywords internal
.anova_es.afex_aov <- function(model,
                               type = c("eta", "omega", "epsilon"),
                               partial = TRUE,
                               generalized = FALSE,
                               ci = 0.95, alternative = "greater",
                               verbose = TRUE,
                               include_intercept = FALSE,
                               ...) {
  type <- match.arg(type)
  if (type == "eta" && isTRUE(generalized) && length(attr(model$anova_table, "observed"))) {
    generalized <- attr(model$anova_table, "observed")
  }

  out <-
    .anova_es(
      model$Anova,
      type = type,
      partial = partial,
      generalized = generalized,
      ci = ci, alternative = alternative,
      verbose = FALSE,
      include_intercept = include_intercept,
      ...
    )

  attr(out, "anova_type") <- attr(model, "type", exact = TRUE)
  attr(out, "approximate") <- FALSE
  out
}

#' @keywords internal
.anova_es.mixed <- function(model,
                            type = c("eta", "omega", "epsilon"),
                            partial = TRUE, generalized = FALSE,
                            ci = 0.95, alternative = "greater",
                            verbose = TRUE,
                            include_intercept = FALSE,
                            ...) {
  aov_tab <- as.data.frame(model[["anova_table"]])

  if (!"F" %in% colnames(aov_tab)) {
    insight::format_error("Cannot estimate approx effect size for `mixed` type model - no F-statistic found.")
  }

  if (verbose && include_intercept && !"(Intercept)" %in% rownames(aov_tab)) {
    insight::format_warning("Cannot estimate (Intercept) effect size for `mixed` model.")
    include_intercept <- FALSE
  }

  aov_tab$Parameter <- rownames(aov_tab)
  aov_tab$df <- aov_tab[["num Df"]]
  aov_tab$df_error <- aov_tab[["den Df"]]
  aov_tab <- aov_tab[, c("Parameter", "df", "df_error", "F")]

  out <- .es_aov_table(aov_tab,
    type = type,
    partial = partial, generalized = generalized,
    ci = ci, alternative = alternative,
    verbose = verbose,
    include_intercept = include_intercept, ...
  )

  attr(out, "anova_type") <- attr(model, "type")
  attr(out, "approximate") <- TRUE
  out
}

#' @keywords internal
.anova_es.Anova.mlm <-
  function(model,
           type = c("eta", "omega", "epsilon"),
           partial = TRUE,
           generalized = FALSE,
           ci = 0.95, alternative = "greater",
           verbose = TRUE,
           include_intercept = FALSE,
           ...) {
    suppressWarnings({
      aov_tab <- summary(model)$univariate.tests
    })

    # if there are univariate.tests, will return a global effect size
    if (is.null(aov_tab)) {
      # TODO this should be the method for manova,
      # so this should be copied there, and here happsed to:
      # .anova_es.manova
      aov_tab <- parameters::model_parameters(model, include_intercept = include_intercept)
      aov_tab$df <- aov_tab$df_num
      out <- .anova_es(aov_tab,
        type = type,
        partial = partial, generalized = generalized,
        ci = ci, alternative = alternative,
        include_intercept = include_intercept,
        verbose = verbose
      )
      attr(out, "anova_type") <- as.numeric(utils::as.roman(model$type))
      attr(out, "approximate") <- FALSE
      return(out)
    }

    # Faking the model_parameters.aovlist output:
    aov_tab <- as.data.frame(unclass(aov_tab))
    aov_tab$Parameter <- rownames(aov_tab)
    colnames(aov_tab)[colnames(aov_tab) == "Sum Sq"] <- "Sum_Squares"
    colnames(aov_tab)[colnames(aov_tab) == "num Df"] <- "df"
    aov_tab <- aov_tab[c("Parameter", "Sum_Squares", "Error SS", "df", "den Df")]

    id <- "Subject"
    within_subj <- names(model$idata)
    within_subj <- lapply(within_subj, function(x) c(NA, x))
    within_subj <- do.call(expand.grid, within_subj)
    within_subj <- apply(within_subj, 1, stats::na.omit)
    ns <- lengths(within_subj)
    within_subj <- sapply(within_subj, paste, collapse = ":")
    within_subj <- within_subj[order(ns)]
    within_subj <- Filter(function(x) nzchar(x, keepNA = TRUE), within_subj)
    l <- sapply(within_subj, grepl, x = aov_tab$Parameter, simplify = TRUE)
    l <- apply(l, 1, function(x) if (any(x)) max(which(x)) else 0)
    l <- c(NA, within_subj)[l + 1]
    l <- sapply(l, function(x) paste0(stats::na.omit(c(id, x)), collapse = ":"))
    aov_tab$Group <- l

    aov_tab <- split(aov_tab, aov_tab$Group)
    aov_tab <- lapply(aov_tab, function(x) {
      x <- x[c(seq_len(nrow(x)), 1), ]
      x$Sum_Squares[nrow(x)] <- x[["Error SS"]][1]
      x$df[nrow(x)] <- x[["den Df"]][1]
      x$Parameter[nrow(x)] <- "Residuals"
      x
    })
    aov_tab <- do.call(rbind, aov_tab)
    aov_tab[["Error SS"]] <- NULL
    aov_tab[["den Df"]] <- NULL
    aov_tab[["F"]] <- ifelse(aov_tab$Parameter == "Residuals", NA, 1)
    aov_tab$Mean_Square <- aov_tab$Sum_Squares / aov_tab$df

    DV_names <- c(id, setdiff(unlist(strsplit(model$terms, ":", fixed = TRUE)), "(Intercept)"))

    out <-
      .es_aov_strata(
        aov_tab,
        DV_names = DV_names,
        type = type,
        partial = partial,
        generalized = generalized,
        ci = ci, alternative = alternative,
        verbose = verbose,
        include_intercept = include_intercept
      )
    out$Group <- NULL

    # Reorder rows
    orig_terms <- model$terms
    if (include_intercept && !"(Intercept)" %in% orig_terms) {
      orig_terms <- c("(Intercept)", orig_terms)
    } else if (!include_intercept && "(Intercept)" %in% orig_terms) {
      orig_terms <- setdiff(orig_terms, "(Intercept)")
    }
    out <- out[match(out$Parameter, orig_terms), ]

    attr(out, "anova_type") <- as.numeric(utils::as.roman(model$type))
    attr(out, "approximate") <- FALSE
    out
  }

#' @keywords internal
.anova_es.manova <- function(model, ...) {
  pars <- parameters::model_parameters(model)
  pars$df <- pars$df_num
  pars <- pars[pars$Parameter != "Residuals", ]
  out <- .anova_es(pars, ...)
  attr(out, "anova_type") <- attr(pars, "anova_type")
  attr(out, "approximate") <- TRUE
  out
}


#' @keywords internal
.anova_es.anova.lme <- .anova_es.anova

#' @keywords internal
.anova_es.parameters_model <- function(model,
                                       type = c("eta", "omega", "epsilon"),
                                       partial = TRUE,
                                       generalized = FALSE,
                                       ci = 0.95, alternative = "greater",
                                       verbose = TRUE,
                                       by_response = TRUE,
                                       ...) {
  if (by_response && "Response" %in% colnames(model)) {
    out <- split(model, model[["Response"]])
    out <- lapply(out, .anova_es.parameters_model,
      type = type, partial = partial, generalized = generalized,
      ci = ci, alternative = alternative,
      verbose = verbose,
      by_response = FALSE,
      ...
    )
    saved_attr <- attributes(out[[1]])
    out <- Map(function(x, nm) cbind(Response = nm, x), out, names(out))
    out <- do.call(rbind, out)
    out$Parameter <- as.character(out$Parameter)

    # Set attributes ---
    attr(out, "generalized") <- saved_attr$generalized
    attr(out, "ci") <- saved_attr$ci
    attr(out, "alternative") <- saved_attr$alternative
    attr(out, "anova_type") <- attr(model, "anova_type")
    attr(out, "approximate") <- saved_attr$approximate
    return(out)
  }


  approximate <- FALSE
  if ("Sum_Squares" %in% colnames(model) && "Residuals" %in% model[["Parameter"]]) {
    if ("Group" %in% colnames(model)) {
      DVs <- unlist(insight::find_predictors(.get_object_from_params(model)))
      out <- .es_aov_strata(
        model,
        DV_names = DVs,
        type = type, partial = partial, generalized = generalized,
        ci = ci, alternative = alternative,
        verbose = verbose, ...
      )
    } else {
      out <- .es_aov_simple(
        model,
        type = type, partial = partial, generalized = generalized,
        ci = ci, alternative = alternative,
        verbose = verbose, ...
      )
    }
  } else {
    out <- .es_aov_table(
      model,
      type = type, partial = partial, generalized = generalized,
      ci = ci, alternative = alternative,
      verbose = verbose, ...
    )
    approximate <- TRUE
  }
  attr(out, "anova_type") <- attr(model, "anova_type")
  attr(out, "approximate") <- approximate
  out
}

# Specific models ---------------------------------------------------------

#' @keywords internal
.anova_es.maov <- function(model,
                           type = c("eta", "omega", "epsilon"),
                           partial = TRUE,
                           generalized = FALSE,
                           ci = 0.95, alternative = "greater",
                           verbose = TRUE,
                           ...) {
  ## TODO: add back `effects = "fixed"` once the deprecation warning in parameters is removed
  params <- parameters::model_parameters(model, verbose = verbose, es_type = NULL)
  anova_type <- attr(params, "anova_type")

  params <- split(params, factor(params$Response, levels = unique(params$Response))) # make sure row order is not changed
  params <- lapply(params, .es_aov_simple,
    type = type,
    partial = partial,
    generalized = generalized,
    ci = ci, alternative = alternative,
    verbose = verbose,
    ...
  )

  params <- lapply(names(params), function(nm) {
    cbind(Response = nm, params[[nm]])
  })
  out <- do.call("rbind", params)
  rownames(out) <- NULL
  out$Response <- as.character(out$Response)

  attr(out, "generalized") <- attr(params[[1]], "generalized")
  attr(out, "ci") <- attr(params[[1]], "ci", exact = TRUE)
  attr(out, "anova_type") <- anova_type
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- if (is.numeric(attr(out, "ci"))) alternative
  out
}

#' @keywords internal
.anova_es.mlm <- function(model, ...) {
  .anova_es.maov(stats::aov(model), ...)
}


#' @keywords internal
.anova_es.htest <- function(model,
                            type = c("eta", "omega", "epsilon"),
                            partial = TRUE,
                            generalized = FALSE,
                            ci = 0.95, alternative = "greater",
                            verbose = TRUE,
                            ...) {
  if (!grepl("One-way", model$method, fixed = TRUE)) {
    insight::format_error("'model' is not a one-way test!")
  }

  if (verbose && (partial || isTRUE(generalized) || is.character(generalized))) {
    txt_type <- ifelse(isTRUE(generalized) || is.character(generalized), "generalized", "partial")
    insight::format_alert(
      sprintf(
        "For one-way between subjects designs, %s %s squared is equivalent to %s squared. Returning %s squared.",
        txt_type, type, type, type
      )
    )
  }

  effectsize(model, type = type, ci = ci, alternative = alternative, verbose = verbose, ...)
}

#' @keywords internal
.anova_es.merMod <- function(model,
                             type = c("eta", "omega", "epsilon"),
                             partial = TRUE,
                             generalized = FALSE,
                             ci = 0.95, alternative = "greater",
                             verbose = TRUE,
                             ...) {
  insight::check_if_installed("lmerTest")

  model <- lmerTest::as_lmerModLmerTest(model)
  model <- stats::anova(model)
  out <-
    .anova_es.anova(
      model,
      type = type,
      partial = partial,
      generalized = generalized,
      ci = ci,
      alternative = alternative,
      ...
    )
  attr(out, "approximate") <- TRUE
  out
}

#' @keywords internal
.anova_es.gam <- function(model,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          generalized = FALSE,
                          ci = 0.95, alternative = "greater",
                          verbose = TRUE,
                          ...) {
  model <- stats::anova(model)

  p.table <- as.data.frame(model$pTerms.table)
  p.table$Component <- "conditional"
  s.table <- as.data.frame(model$s.table)
  s.table$Component <- "smooth_terms"
  colnames(s.table)[colnames(s.table) == "Ref.df"] <- "df"
  s.table[setdiff(colnames(p.table), colnames(s.table))] <- NA
  p.table[setdiff(colnames(s.table), colnames(p.table))] <- NA
  tab <- rbind(p.table, s.table)
  colnames(tab)[colnames(tab) == "df"] <- "npar"
  tab$df_error <- model$residual.df
  # tab$df_error <- Inf

  out <-
    .anova_es.anova(
      tab,
      type = type,
      generalized = generalized,
      partial = partial,
      ci = ci, alternative = alternative,
      verbose = verbose
    )
  out$Component <- tab$Component
  out <- datawizard::data_relocate(out, select = "Component", before = 1)

  attr(out, "anova_type") <- 3
  attr(out, "approximate") <- TRUE
  out
}


#' @keywords internal
.anova_es.rms <- function(model,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          generalized = FALSE,
                          ci = 0.95, alternative = "greater",
                          verbose = TRUE,
                          ...) {
  if (!inherits(model, "anova.rms")) {
    model <- stats::anova(model, test = "F")
  }

  i <- rownames(model)
  model <- as.data.frame(model)
  model$Parameter <- i
  colnames(model) <- gsub("d.f.", "df", colnames(model), fixed = TRUE)
  model$df_error <- model$df[rownames(model) == "ERROR"]
  model <- model[rownames(model) != "ERROR", ]

  out <- .es_aov_table(
    model,
    type = type,
    partial = partial,
    generalized = generalized,
    ci = ci,
    alternative = alternative,
    ...
  )
  attr(out, "anova_type") <- 2
  attr(out, "approximate") <- FALSE
  out
}

.anova_es.anova.rms <- .anova_es.rms


#' @export
.anova_es.model_fit <- function(model, ...) {
  .anova_es(model$fit, ...)
}


# Utils -------------------------------------------------------------------

#' @keywords internal
.get_object_from_params <- function(x, attribute_name = "object_name") {
  obj_name <- attr(x, attribute_name, exact = TRUE)
  model <- NULL
  if (!is.null(obj_name)) {
    model <- tryCatch(
      {
        get(obj_name, envir = parent.frame())
      },
      error = function(e) {
        NULL
      }
    )
    if (is.null(model) ||
      # prevent self reference
      inherits(model, "parameters_model")) {
      model <- tryCatch(
        {
          get(obj_name, envir = globalenv())
        },
        error = function(e) {
          NULL
        }
      )
    }
  }
  model
}
