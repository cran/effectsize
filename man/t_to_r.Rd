% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_tFz_to_d.R, R/convert_tFz_to_r.R
\name{t_to_d}
\alias{t_to_d}
\alias{z_to_d}
\alias{F_to_d}
\alias{t_to_r}
\alias{z_to_r}
\alias{F_to_r}
\title{Convert test statistics (t, z, F) to effect sizes of differences (Cohen's d)
or association (\strong{partial} r)}
\usage{
t_to_d(t, df_error, paired = FALSE, ci = 0.95, pooled, ...)

z_to_d(z, n, paired = FALSE, ci = 0.95, pooled, ...)

F_to_d(f, df, df_error, paired = FALSE, ci = 0.95, ...)

t_to_r(t, df_error, ci = 0.95, ...)

z_to_r(z, n, ci = 0.95, ...)

F_to_r(f, df, df_error, ci = 0.95, ...)
}
\arguments{
\item{t, f, z}{The t, the F or the z statistics.}

\item{paired}{Should the estimate account for the t-value being testing the
difference between dependent means?}

\item{ci}{Confidence Interval (CI) level}

\item{pooled}{Deprecated. Use \code{paired}.}

\item{...}{Arguments passed to or from other methods.}

\item{n}{The number of observations (the sample size).}

\item{df, df_error}{Degrees of freedom of numerator or of the error estimate
(i.e., the residuals).}
}
\value{
A data frame with the effect size(s)(\code{r} or \code{d}), and their CIs
(\code{CI_low} and \code{CI_high}).
}
\description{
These functions are convenience functions to convert t, z and F test
statistics to Cohen's d and \strong{partial} r. These are useful in cases where
the data required to compute these are not easily available or their
computation is not straightforward (e.g., in liner mixed models, contrasts,
etc.).
\cr
See \href{https://easystats.github.io/effectsize/articles/from_test_statistics.html}{Effect Size from Test Statistics vignette.}
}
\details{
These functions use the following formulae to approximate \emph{r} and \emph{d}:
\cr\cr
\deqn{r_{partial} = t / \sqrt{t^2 + df_{error}}}
\cr\cr
\deqn{r_{partial} = z / \sqrt{z^2 + N}}
\cr\cr
\deqn{d = 2 * t / \sqrt{df_{error}}}
\cr\cr
\deqn{d_z = t / \sqrt{df_{error}}}
\cr\cr
\deqn{d = 2 * z / \sqrt{N}}

The resulting \code{d} effect size is an \emph{approximation} to Cohen's \emph{d}, and
assumes two equal group sizes. When possible, it is advised to directly
estimate Cohen's \emph{d}, with \code{\link[=cohens_d]{cohens_d()}}, \code{emmeans::eff_size()}, or similar
functions.
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

\examples{
## t Tests
res <- t.test(1:10, y = c(7:20), var.equal = TRUE)
t_to_d(t = res$statistic, res$parameter)
t_to_r(t = res$statistic, res$parameter)

res <- with(sleep, t.test(extra[group == 1], extra[group == 2], paired = TRUE))
t_to_d(t = res$statistic, res$parameter, paired = TRUE)
t_to_r(t = res$statistic, res$parameter)
\donttest{
## Linear Regression
model <- lm(rating ~ complaints + critical, data = attitude)
library(parameters)
(param_tab <- parameters(model))

(rs <- t_to_r(param_tab$t[2:3], param_tab$df_error[2:3]))

if (require(see)) plot(rs)

# How does this compare to actual partial correlations?
if (require("correlation")) {
  correlation::correlation(attitude[, c(1, 2, 6)], partial = TRUE)[1:2, c(2, 3, 7, 8)]
}

## Use with emmeans based contrasts (see also t_to_eta2)
if (require(emmeans)) {
  warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)


  # Also see emmeans::eff_size()
  em_tension <- emmeans(warp.lm, ~tension) #'
  diff_tension <- summary(pairs(em_tension))
  t_to_d(diff_tension$t.ratio, diff_tension$df)
}
}

}
\references{
\itemize{
\item Friedman, H. (1982). Simplified determinations of statistical power,
magnitude of effect and research sample sizes. Educational and Psychological
Measurement, 42(2), 521-526. \doi{10.1177/001316448204200214}
\item Wolf, F. M. (1986). Meta-analysis: Quantitative methods for research
synthesis (Vol. 59). Sage.
\item Rosenthal, R. (1994) Parametric measures of effect size. In H. Cooper and
L.V. Hedges (Eds.). The handbook of research synthesis. New York: Russell
Sage Foundation.
\item Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals
and tests of close fit in the analysis of variance and contrast analysis.
Psychological Methods, 9, 164-182.
\item Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and
calculation of confidence intervals that are based on central and noncentral
distributions. Educational and Psychological Measurement, 61(4), 532-574.
}
}
\seealso{
Other effect size from test statistic: 
\code{\link{F_to_eta2}()},
\code{\link{chisq_to_phi}()}
}
\concept{effect size from test statistic}
