% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cohens_d.R
\name{cohens_d}
\alias{cohens_d}
\alias{hedges_g}
\alias{glass_delta}
\title{Cohen's \emph{d} and Other Standardized Differences}
\usage{
cohens_d(
  x,
  y = NULL,
  data = NULL,
  pooled_sd = TRUE,
  mu = 0,
  paired = FALSE,
  reference = NULL,
  adjust = FALSE,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)

hedges_g(
  x,
  y = NULL,
  data = NULL,
  pooled_sd = TRUE,
  mu = 0,
  paired = FALSE,
  reference = NULL,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)

glass_delta(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  adjust = TRUE,
  reference = NULL,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)
}
\arguments{
\item{x, y}{A numeric vector, or a character name of one in \code{data}.
Any missing values (\code{NA}s) are dropped from the resulting vector.
\code{x} can also be a formula (see \code{\link[stats:t.test]{stats::t.test()}}), in which case \code{y} is
ignored.}

\item{data}{An optional data frame containing the variables.}

\item{pooled_sd}{If \code{TRUE} (default), a \code{\link[=sd_pooled]{sd_pooled()}} is used (assuming equal
variance). Else the mean SD from both groups is used instead.}

\item{mu}{a number indicating the true value of the mean (or
    difference in means if you are performing a two sample test).}

\item{paired}{If \code{TRUE}, the values of \code{x} and \code{y} are considered as paired.
This produces an effect size that is equivalent to the one-sample effect
size on \code{x - y}. See also \code{\link[=repeated_measures_d]{repeated_measures_d()}} for more options.}

\item{reference}{(Optional) character value of the "group" used as the
reference. By default, the \emph{second} group is the reference group.}

\item{adjust}{Should the effect size be adjusted for small-sample bias using
Hedges' method? Note that \code{hedges_g()} is an alias for
\code{cohens_d(adjust = TRUE)}.}

\item{ci}{Confidence Interval (CI) level}

\item{alternative}{a character string specifying the alternative hypothesis;
Controls the type of CI returned: \code{"two.sided"} (default, two-sided CI),
\code{"greater"} or \code{"less"} (one-sided CI). Partial matching is allowed (e.g.,
\code{"g"}, \code{"l"}, \code{"two"}...). See \emph{One-Sided CIs} in \link{effectsize_CIs}.}

\item{verbose}{Toggle warnings and messages on or off.}

\item{...}{Arguments passed to or from other methods. When \code{x} is a formula,
these can be \code{subset} and \code{na.action}.}
}
\value{
A data frame with the effect size ( \code{Cohens_d}, \code{Hedges_g},
\code{Glass_delta}) and their CIs (\code{CI_low} and \code{CI_high}).
}
\description{
Compute effect size indices for standardized mean differences: Cohen's \emph{d},
Hedges' \emph{g} and Glass’s \emph{delta} (\eqn{\Delta}). (This function returns the
\strong{population} estimate.) Pair with any reported \code{\link[stats:t.test]{stats::t.test()}}.
\cr\cr
Both Cohen's \emph{d} and Hedges' \emph{g} are the estimated the standardized
difference between the means of two populations. Hedges' \emph{g} provides a
correction for small-sample bias (using the exact method) to Cohen's \emph{d}. For
sample sizes > 20, the results for both statistics are roughly equivalent.
Glass’s \emph{delta} is appropriate when the standard deviations are significantly
different between the populations, as it uses only the reference group's
standard deviation.
}
\details{
Set \code{pooled_sd = FALSE} for effect sizes that are to accompany a Welch's
\emph{t}-test (Delacre et al, 2021).
}
\note{
The indices here give the population estimated standardized difference.
Some statistical packages give the sample estimate instead (without
applying Bessel's correction).
}
\section{Confidence (Compatibility) Intervals (CIs)}{
Unless stated otherwise, confidence (compatibility) intervals (CIs) are
estimated using the noncentrality parameter method (also called the "pivot
method"). This method finds the noncentrality parameter ("\emph{ncp}") of a
noncentral \emph{t}, \emph{F}, or \eqn{\chi^2} distribution that places the observed
\emph{t}, \emph{F}, or \eqn{\chi^2} test statistic at the desired probability point of
the distribution. For example, if the observed \emph{t} statistic is 2.0, with 50
degrees of freedom, for which cumulative noncentral \emph{t} distribution is \emph{t} =
2.0 the .025 quantile (answer: the noncentral \emph{t} distribution with \emph{ncp} =
.04)? After estimating these confidence bounds on the \emph{ncp}, they are
converted into the effect size metric to obtain a confidence interval for the
effect size (Steiger, 2004).
\cr\cr
For additional details on estimation and troubleshooting, see \link{effectsize_CIs}.
}

\section{CIs and Significance Tests}{
"Confidence intervals on measures of effect size convey all the information
in a hypothesis test, and more." (Steiger, 2004). Confidence (compatibility)
intervals and p values are complementary summaries of parameter uncertainty
given the observed data. A dichotomous hypothesis test could be performed
with either a CI or a p value. The 100 (1 - \eqn{\alpha})\% confidence
interval contains all of the parameter values for which \emph{p} > \eqn{\alpha}
for the current data and model. For example, a 95\% confidence interval
contains all of the values for which p > .05.
\cr\cr
Note that a confidence interval including 0 \emph{does not} indicate that the null
(no effect) is true. Rather, it suggests that the observed data together with
the model and its assumptions combined do not provided clear evidence against
a parameter value of 0 (same as with any other value in the interval), with
the level of this evidence defined by the chosen \eqn{\alpha} level (Rafi &
Greenland, 2020; Schweder & Hjort, 2016; Xie & Singh, 2013). To infer no
effect, additional judgments about what parameter values are "close enough"
to 0 to be negligible are needed ("equivalence testing"; Bauer & Kiesser,
1996).
}

\section{Plotting with \code{see}}{

The \code{see} package contains relevant plotting functions. See the \href{https://easystats.github.io/see/articles/effectsize.html}{plotting vignette in the \code{see} package}.
}

\examples{
\donttest{
data(mtcars)
mtcars$am <- factor(mtcars$am)

# Two Independent Samples ----------

(d <- cohens_d(mpg ~ am, data = mtcars))
# Same as:
# cohens_d("mpg", "am", data = mtcars)
# cohens_d(mtcars$mpg[mtcars$am=="0"], mtcars$mpg[mtcars$am=="1"])

# More options:
cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
cohens_d(mpg ~ am, data = mtcars, mu = -5)
cohens_d(mpg ~ am, data = mtcars, alternative = "less")
hedges_g(mpg ~ am, data = mtcars)
glass_delta(mpg ~ am, data = mtcars)


# One Sample ----------

cohens_d(wt ~ 1, data = mtcars)

# same as:
# cohens_d("wt", data = mtcars)
# cohens_d(mtcars$wt)

# More options:
cohens_d(wt ~ 1, data = mtcars, mu = 3)
hedges_g(wt ~ 1, data = mtcars, mu = 3)


# Paired Samples ----------

data(sleep)

cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep)

# same as:
# cohens_d(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], paired = TRUE)
# cohens_d(sleep$extra[sleep$group == 1] - sleep$extra[sleep$group == 2])
# rm_d(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], method = "z", adjust = FALSE)

# More options:
cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, mu = -1, verbose = FALSE)
hedges_g(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, verbose = FALSE)


# Interpretation -----------------------
interpret_cohens_d(-1.48, rules = "cohen1988")
interpret_hedges_g(-1.48, rules = "sawilowsky2009")
interpret_glass_delta(-1.48, rules = "gignac2016")
# Or:
interpret(d, rules = "sawilowsky2009")

# Common Language Effect Sizes
d_to_u3(1.48)
# Or:
print(d, append_CLES = TRUE)
}

}
\references{
\itemize{
\item Algina, J., Keselman, H. J., & Penfield, R. D. (2006). Confidence intervals
for an effect size when variances are not equal. Journal of Modern Applied
Statistical Methods, 5(1), 2.
\item Cohen, J. (1988). Statistical power analysis for the behavioral
sciences (2nd Ed.). New York: Routledge.
\item Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021, May 7). Why
Hedges’ g*s based on the non-pooled standard deviation should be reported
with Welch's t-test. \doi{10.31234/osf.io/tu6mp}
\item Hedges, L. V. & Olkin, I. (1985). Statistical methods for
meta-analysis. Orlando, FL: Academic Press.
\item Hunter, J. E., & Schmidt, F. L. (2004). Methods of meta-analysis:
Correcting error and bias in research findings. Sage.
}
}
\seealso{
\code{\link[=rm_d]{rm_d()}}, \code{\link[=sd_pooled]{sd_pooled()}}, \code{\link[=t_to_d]{t_to_d()}}, \code{\link[=r_to_d]{r_to_d()}}

Other standardized differences: 
\code{\link{mahalanobis_d}()},
\code{\link{means_ratio}()},
\code{\link{p_superiority}()},
\code{\link{rank_biserial}()},
\code{\link{repeated_measures_d}()}
}
\concept{standardized differences}
