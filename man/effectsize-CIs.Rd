% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docs_extra.R
\name{effectsize-CIs}
\alias{effectsize-CIs}
\title{Confidence Intervals}
\description{
More information regarding Confidence Intervals and how they are computed in
\code{effectsize}.
}
\section{Confidence Intervals}{

Unless stated otherwise, confidence intervals are estimated using the
Noncentrality parameter method; These methods searches for a the best
non-central parameters (\code{ncp}s) of the noncentral t-, F- or Chi-squared
distribution for the desired tail-probabilities, and then convert these
\code{ncp}s to the corresponding effect sizes. (See full \link{effectsize-CIs} for
more.)
}

\section{CI Contains Zero}{

Keep in mind that \code{ncp} confidence intervals are inverted significance tests,
and only inform us about which values are not significantly different than
our sample estimate. (They do \emph{not} inform us about which values are
plausible, likely or compatible with our data.) Thus, when CIs contain the
value 0, this should \emph{not} be taken to mean that a null effect size is
supported by the data; Instead this merely reflects a non-significant test
statistic - i.e. the \emph{p}-value is greater than alpha (Morey et al., 2016).
\cr\cr
For positive only effect sizes (Eta squared, Cramer's V, etc.; Effect sizes
associated with Chi-squared and F distributions), this applies also to cases
where the lower bound of the CI is equal to 0. Even more care should be taken
when the \emph{upper} bound is equal to 0 - this occurs when \emph{p}-value is greater
than 1-alpha/2 making, the upper bound cannot be estimated, and the upper
bound is arbitrarily set to 0 (Steiger, 2004). For example:\if{html}{\out{<div class="r">}}\preformatted{eta_squared(aov(mpg ~ factor(gear) + factor(cyl), mtcars[1:7, ]))
}\if{html}{\out{</div>}}\preformatted{## # Effect Size for ANOVA (Type I)
## 
## Parameter    | Eta2 (partial) |       90\% CI
## --------------------------------------------
## factor(gear) |           0.58 | [0.00, 0.84]
## factor(cyl)  |           0.46 | [0.00, 0.78]
}
}

\section{CI Does Not Contain the Estimate}{

For very large sample sizes, the width of the CI can be smaller than the
tolerance of the optimizer, resulting in CIs of width 0. This can also,
result in the estimated CIs excluding the point estimate. For example:\if{html}{\out{<div class="r">}}\preformatted{chisq_to_cramers_v(13223.73, n = 76227, nrow = 6, ncol = 1)
}\if{html}{\out{</div>}}\preformatted{## Cramer's V |       95\% CI
## -------------------------
## 0.19       | [0.20, 0.20]
}\if{html}{\out{<div class="r">}}\preformatted{t_to_d(80, df_error = 4555555)
}\if{html}{\out{</div>}}\preformatted{## d    |       95\% CI
## -------------------
## 0.07 | [0.08, 0.08]
}
}

\references{
\itemize{
\item Morey, R. D., Hoekstra, R., Rouder, J. N., Lee, M. D., & Wagenmakers, E. J. (2016). The fallacy of placing confidence in confidence intervals. Psychonomic bulletin & review, 23(1), 103-123.
\item Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. Psychological Methods, 9, 164-182.
}
}
