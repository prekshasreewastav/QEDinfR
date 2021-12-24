#'@title Generic Method for Outcome Analysis
#'
#'@description
#' \strong{test} is a generic function for hypothesis testing/outcome analysis. The function invokes particular methods which depend on the class of the first argument.
#'
#'@param object an object to be tested
#'
#'@export
#'
#'
#'@details
#'As of now, this generic method supports outcome analysis for matched objects using the \strong{QEDinfR} package.
#'
#'@return The function \strong{test} computes and prints outcome analysis or hypothesis testing for various object classes.
#'
#'
#'
#'@seealso
#'\code{\link[QEDinfR]{test.qed_match}}
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
