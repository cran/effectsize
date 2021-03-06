% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sd_pooled.R
\name{sd_pooled}
\alias{sd_pooled}
\alias{mad_pooled}
\title{Pooled Standard Deviation}
\usage{
sd_pooled(x, y = NULL, data = NULL, verbose = TRUE)

mad_pooled(x, y = NULL, data = NULL, constant = 1.4826, verbose = TRUE)
}
\arguments{
\item{x}{A formula, a numeric vector, or a character name of one in \code{data}.}

\item{y}{A numeric vector, a grouping (character / factor) vector, a or a
character  name of one in \code{data}. Ignored if \code{x} is a formula.}

\item{data}{An optional data frame containing the variables.}

\item{verbose}{Toggle warnings and messages on or off.}

\item{constant}{scale factor.}
}
\value{
Numeric, the pooled standard deviation.
}
\description{
The Pooled Standard Deviation is a weighted average of standard deviations
for two or more groups, \emph{assumed to have equal variance}. It represents the
common deviation among the groups, around each of their respective means.
}
\details{
The standard version is calculated as:
\deqn{\sqrt{\frac{\sum (x_i - \bar{x})^2}{n_1 + n_2 - 2}}}{sqrt(sum(c(x - mean(x), y - mean(y))^2) / (n1 + n2 - 2))}
The robust version is calculated as:
\deqn{1.4826 \times Median(|\left\{x - Median_x,\,y - Median_y\right\}|)}{mad(c(x - median(x), y - median(y)), constant = 1.4826)}
}
\examples{
sd_pooled(mpg ~ am, data = mtcars)
mad_pooled(mtcars$mpg, factor(mtcars$am))
}
\seealso{
\code{\link[=cohens_d]{cohens_d()}}
}
