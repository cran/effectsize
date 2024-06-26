% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{screening_test}
\alias{screening_test}
\title{Results from 2 Screening Tests}
\format{
A data frame with 1600 rows and 3 variables:
\describe{
\item{Diagnosis}{Ground truth}
\item{Test1}{Results given by the 1st test}
\item{Test2}{Results given by the 2nd test}
}

\if{html}{\out{<div class="sourceCode r">}}\preformatted{data("screening_test")
head(screening_test, n = 5)
#>   Diagnosis Test1 Test2
#> 1       Neg "Neg" "Neg"
#> 2       Neg "Neg" "Neg"
#> 3       Neg "Neg" "Neg"
#> 4       Neg "Neg" "Neg"
#> 5       Neg "Neg" "Neg"
}\if{html}{\out{</div>}}
}
\description{
A sample (simulated) dataset, used in tests and some examples.
}
\seealso{
Other effect size datasets: 
\code{\link{Music_preferences}},
\code{\link{Music_preferences2}},
\code{\link{RCT_table}},
\code{\link{Smoking_FASD}},
\code{\link{food_class}},
\code{\link{hardlyworking}},
\code{\link{rouder2016}}
}
\concept{effect size datasets}
\keyword{data}
