% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.qed_match.R
\name{summary.qed_match}
\alias{summary.qed_match}
\title{Generic summarizing function for "qed_match" objects}
\usage{
\method{summary}{qed_match}(qed_match, ...)
}
\arguments{
\item{qed_match}{an object of class "qed_match", a result of a call to \link[QEDinfR]{qed_match} (which is a wrapper for \link[Matching]{Match})}
}
\value{
The function \strong{summary.qed_match} computes and prints summary statistics for data that Greedy Matching has been performed on using the \link[QEDinfR]{qed_match} function.
Summary statistics are computed using the \link[tableone]{CreateTableOne} function in the \strong{tableone} package, and \link[Matching]{summary.Match} (which is a summary method for class "Match")
function in the \strong{Matching} package.
}
\description{
Summary method for class "qed_match", which provides relevant pre-matched and post-matched summary statistics.
}
\details{
Standardized Mean Differences are the differences in means between groups (i.e. treatment and control), divided by the (pooled) standard deviation.
Rules of thumb:
\describe{
\item{\strong{SMD < 0.1}}{indicates adequate balance}
\item{\strong{SMD 0.1-0.2}}{indicates moderate balance}
\item{\strong{SMD > 0.2}}{indicates imbalance}
}
}
\examples{
data(lalonde)
# For matching with propensity scores:
ps_match <- qed_match(treat ~ age + educ + black, "re78", lalonde, "propensityscore", caliper = NULL, replace = FALSE)
# For matching with Mahalanobis distances:
md_match <- qed_match(treat ~ age + educ + black, "re78", lalonde, "mahalanobis", caliper = NULL, replace = FALSE)

# Summary statistics:
summary(ps_match)
summary(md_match)

}
\seealso{
\code{\link[Matching]{summary.Match}},
\code{\link[tableone]{CreateTableOne}},
\code{\link[QEDinfR]{qed_match}},
\code{\link[QEDinfR]{test.qed_match}}
}
