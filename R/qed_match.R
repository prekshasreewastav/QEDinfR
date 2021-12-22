
qed_match <- function(formula, outcome, data, distance, caliper = NULL, replace = FALSE, ...){

  #distance = propensity score
      if(distance == "propensityscore")
        {
      #calculate propensity scores using logistic regression
      ps.model <- glm(formula,
                      family = binomial(),
                      data = data)
      ps.scores <- ps.model$fitted.values

      #perform greedy matching using Match in Matching package
      ps.match <- Matching::Match(Y = data[,outcome], Tr = data[[as.character(formula[[2]])]], X = ps.scores, caliper = caliper, replace = replace)

      #add matched observations from data to results
      matched <- data[unlist(ps.match[c("index.treated","index.control")]), ]
      ps.post <- glm(formula,
                            family = binomial(),
                            data = matched)
      ps.scores.post <- ps.post$fitted.values

      result <- append((ps.match), list("Matched" = matched, "Call" = formula, "Distance" = "Propensity Score", "Data" = data, "Pre-PS" = ps.scores, "Post-PS" = ps.scores.post))
      class(result) <- c("qed_match", "Match")

      invisible(result)
      }

    #distance = mahalanobis
    else
        {
      #X vars matrix
      xvars <- (formula[[3]])
      xvars1 <- gsub("[+]", "", as.character(xvars[2]))
      xvars1 <- unlist(tokenizers::tokenize_word_stems(xvars1))
      xvars_last <- as.character(xvars[3])
      vars <- append(xvars1, xvars_last)

      maha.match <- Matching::Match(Y = data[[outcome]], Tr = data[[as.character(formula[[2]])]], X = data[vars], caliper = caliper, replace = replace)

      #add matched observations from data to results
      matched <- data[unlist(maha.match[c("index.treated","index.control")]), ]

      result <- append((maha.match), list("Matched" = matched, "Call" = formula, "Distance" = "Mahalanobis", "Data" = data))
      class(result) <- c("qed_match", "Match")

      invisible(result)
      }


}

a_m <- qed_match(treat ~ age + educ + black + hisp + married, "re78", lalonde, "mahalanobis")
a_p <- qed_match(treat ~ age + educ + black + hisp + married, "re78", lalonde, "propensityscore")
b <- Match(lalonde$re78, lalonde$treat, lalonde[1:5])
