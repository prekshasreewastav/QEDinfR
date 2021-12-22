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

summary(a_m)
summary(a_p)

