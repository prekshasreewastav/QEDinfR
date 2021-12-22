#'@title Summary.qed_match: Summarazing Matching statistics
#'
#'@description
#'Summary method for class "qed_match", which provides relevant pre-matched and post-matched summary statistics.
#'
#'@param qed_match an object of class "qed_match", a result of a call to \link[QEDinfR]{qed_match} (which is a wrapper for \link[Matching]{Match})
#'
#'@export
#'
#'@importFrom tokenizers tokenize_word_stems
#'@importFrom tableone CreateTableOne
#'@importFrom Matching summary.Match
#'
#'@details Standardized Mean Differences are the differences in means between groups (i.e. treatment and control), divided by the (pooled) standard deviation.
#'Rules of thumb:
#'\describe{
#'   \item{\strong{SMD < 0.1}}{indicates adequate balance}
#'   \item{\strong{SMD 0.1-0.2}}{indicates moderate balance}
#'   \item{\strong{SMD > 0.2}}{indicates imbalance}
#'}
#'
#'@return The function \strong{summary.qed_match} computes and prints summary statistics for data that Greedy Matching has been performed on using the \link[QEDinfR]{qed_match} function.
#' Summary statistics are computed using the \link[tableone]{CreateTableOne} function in the \strong{tableone} package, and \link[Matching]{summary.Match} (which is a summary method for class "Match")
#'function in the \strong{Matching} package.
#'
#'
#'@seealso \code{\link[Matching]{summary.Match}},
#'\code{\link[tableone]{CreateTableOne}},
#'\code{\link[QEDinfR]{qed_match}},
#'\code{\link[QEDinfR]{test.qed_match}}
#'
#'@examples
#'\dontrun{
#'data(lalonde)
#'# For matching with propensity scores:
#'ps_match <- qed_match(treat ~ age + educ + black, "re78", lalonde, "propensityscore", caliper = NULL, replace = FALSE)
#'# For matching with Mahalanobis distances:
#'md_match <- qed_match(treat ~ age + educ + black, "re78", lalonde, "mahalanobis", caliper = NULL, replace = FALSE)
#'
#' # Summary statistics:
#' summary(ps_match)
#' summary(md_match)
#'}

summary.qed_match <- function(qed_match, ...){

  #Arguments for CreateTableOne()
  #X-variables
  xvars <- (qed_match$Call[[3]])
  xvars1 <- gsub("[+]", "", as.character(xvars[2]))
  xvars1 <- unlist(tokenizers::tokenize_word_stems(xvars1))
  xvars_last <- as.character(xvars[3])
  vars <- append(xvars1, xvars_last)
  #Treatment variable
  treat <- as.character(qed_match$Call[[2]])


  #Using CreateTableOne from package tableone
  summary_pre <- tableone::CreateTableOne(vars, treat, qed_match[[27]], test = FALSE)
  summary <- tableone::CreateTableOne(vars, treat, qed_match[[24]], test = FALSE)

  #Print
  cat("---------------------------------------------", "\n",
      "       Matching Summary Statistics", "\n",
      "---------------------------------------------", "\n", sep = "")
  cat("Method: Greedy Matching", "\n",
      "Distance Measure: ", qed_match$Distance, "\n", "\n", sep="")
  cat("Variables used for Matching:", "\n", summary$MetaData$vars, "\n", "\n", sep=" ")
  cat("% Missing Values:", "\n")
  print(summary$MetaData$percentMissing)
  cat("\n", "Summary:", sep="")
  #Using Summary from package Matching
  class(qed_match) <- c("Match")
  (summary.Match(qed_match))
  cat("\n", "Standardized Mean Differences:", "\n", "  Pre-Matched - ", "\n", sep="")
  print(summary_pre, smd = TRUE)
  cat("\n", "  Post-Matched - ", "\n", sep = "")
  print(summary, smd = TRUE)

}


