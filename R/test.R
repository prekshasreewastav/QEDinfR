#'@title Generic Method for Outcome Analysis for Matched data (to estimate treatment effects)
#'
#'@description
#'Testing method for object of class "qed_match".
#'
#'@param qed_match an object of class "qed_match", a result of a call to \link[QEDinfR]{qed_match} (which is a wrapper for \link[Matching]{Match})
#'
#'@export
#'
#'@importFrom exact2x2 mcnemar.exact
#'
#'
#'@details
#'For numeric outcomes, we perform a t-test to check for significant differences in means between groups.
#'For categorical outcomes, we perform a McNemar test to check for odds ratios for the treatment outcomes and control outcomes.
#'
#'@return The function \strong{test.qed_match} computes and prints outcome analysis to estimate treatment effects for data that Greedy Matching has been performed on using the \link[QEDinfR]{qed_match} function.
#'It also provides an easy interpretation of test results.
#'
#'
#'@seealso \code{\link[exact2x2]{mcnemar.exact}}
#'\code{\link[base]{t.test}}
#'\code{\link[QEDinfR]{qed_match}}
#'
#'@examples
#'data(lalonde)
#'# For matching with propensity scores and a categorical output:
#'lalonde$nodegr <- factor(lalonde$nodegr)
#'ps_match <- qed_match(treat ~ age + educ + black, "nodegr", lalonde, "propensityscore", caliper = NULL, replace = FALSE)
#'# For matching with Mahalanobis distances and a numeric output:
#'md_match <- qed_match(treat ~ age + educ + black, "re78", lalonde, "mahalanobis", caliper = NULL, replace = FALSE)
#'
#'# Outcome Analysis
#'# McNemar test for categorical outcomes:
#'test(ps_match)
#'# T-test for numeric outcomes:
#'test(md_match)
#'

test <- function(x, ...) UseMethod("test")
