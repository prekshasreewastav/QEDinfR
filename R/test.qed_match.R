#'@title Outcome Analysis for for "qed_match" objects
#'
#'@description
#'Testing method for object of class "qed_match" that allows one to estimate treatment effects from matched data.
#'
#'@param qed_match an object of class "qed_match", a result of a call to \link[QEDinfR]{qed_match} (which is a wrapper for \link[Matching]{Match})
#'
#'@export
#'
#'@importFrom exact2x2 mcnemar.exact
#'@importFrom exact2x2 exact2x2
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

test.qed_match <- function(qed_match, ...){

  #Treatment variable
  treat <- as.character(qed_match$Call[[2]])

  #Outcome variable
  outcome <- qed_match$Outcome

  #Matched Data
  matched <- qed_match$Matched
  trt <- matched[,outcome][matched[, treat] == 1]
  con <- matched[,outcome][matched[, treat] == 0]

  #If categorical outcome, McNemar Test
  if(is.factor(matched[,outcome]) == TRUE){
    test <- exact2x2::mcnemar.exact(table(con, trt))
    cat("---------------------------------------", "\n",
        "    Categorical Outcome: ", outcome, "\n",
        "---------------------------------------", "\n", "\n", sep="")

    print(test)
    cat("---------------------------------------", "\n")

    if(test$p.value < 0.001)
      {cat("The association is statistically significant at the 0.001 level. ", "(p-value = ",
                                test$p.value, ")", "\n", sep="")}
    else if(test$p.value < 0.01)
    {cat("The association is statistically significant at the 0.01 level. ", "(p-value = ",
         test$p.value, ")", "\n", sep="")}
    else if(test$p.value < 0.05)
    {cat("The association is statistically significant at the 0.05 level. ", "(p-value = ",
         test$p.value, ")", "\n", sep="")}
    else if(test$p.value < 0.1)
    {cat("The association is statistically significant at the 0.1 level. ", "(p-value = ",
         test$p.value, ")", "\n", sep="")}
    else
    {cat("The association is not statistically significant at convetional levels. ", "(p-value = ",
         test$p.value, ")", "\n", sep="")}


    if(test$estimate > 1)
      {cat("Observations in the treated group are ", test$estimate, " times more likely to be a positive case* ",
                              "\n", "*(i.e. belong to outcome level = 1).", sep="")}
    else{cat("Observations in the treated group are ", test$estimate, " as likely to be a positive case* ",
             "\n", "*(i.e. belong to outcome (", outcome,  ") level = 1).", sep="")}
  }

  #If numeric outcome, T-test
  else {
    test <- t.test(con, trt, paired=TRUE)
    cat("---------------------------------------", "\n",
        "    Numeric Outcome: ", outcome, "\n",
        "---------------------------------------", "\n", sep="")
    print(test)
    cat("---------------------------------------", "\n")

    if(test$p.value < 0.001)
    {cat("The association is statistically significant at the 0.001 level. ", "(p-value = ",
         test$p.value, ")", "\n", sep="")}
    else if(test$p.value < 0.01)
    {cat("The association is statistically significant at the 0.01 level. ", "(p-value = ",
         test$p.value, ")", "\n", sep="")}
    else if(test$p.value < 0.05)
    {cat("The association is statistically significant at the 0.05 level. ", "(p-value = ",
         test$p.value, ")", "\n", sep="")}
    else if(test$p.value < 0.1)
    {cat("The association is statistically significant at the 0.1 level. ", "(p-value = ",
         test$p.value, ")", "\n", sep="")}
    else
    {cat("The association is not statistically significant at convetional levels. ", "(p-value = ",
         test$p.value, ")", "\n", sep="")}

    if(test$estimate > 0){flag = "above"}
    else{flag = "below"}
    cat("On average, observations in the treated group are ", test$estimate, " units ", flag,
        " the control group for the outcome (", outcome,  ").", sep="")
  }


}

