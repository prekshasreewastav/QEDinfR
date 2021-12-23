#'@title qed_match: Greedy Matching (with Propensity Scores or Mahalanobis Distances)
#'
#'@description
#'
#'Performs Greedy Matching (Nearest Neighbor Matching) in order to balance covariates in observational data
#'for causal inference.
#'
#'@param formula formula, in the format: treatment_variable ~ c(covariates)
#'@param outcome string, name of outcome variable of interest
#'@param data dataframe or tibble, data to be used for matching
#'@param distance string, either "mahalanobis" or "propensityscore" to indicate the distance measure (i.e. criterion mataching is performed on)
#'@param caliper numeric, indicates the maximum possible distance a match can be made on (by default, NULL)
#'@param replace logical, indicates whether or not matching must be performed with replacement or without replacement (by default, FALSE)
#'
#'
#'@export
#'
#'@importFrom Matching Match
#'@importFrom tokenizers tokenize_word_stems
#'
#'@details
#'This function serves as a wrapper for the \code{\link[Matching]{Match}} function in the \strong{Matching} package, and restricts distance measures to Mahalanobis distances
#'and Propensity Scores (calculated through a logistic regression). It aims to balance the treatment and control groups on all included covariates in order to perform causal analysis.
#'
#'
#'@return an object of class \code{qed_match} that includes outputs from the Match function, along with:
#'\describe{
#'   \item{\strong{Matched}}{a dataframe containing matched observations}
#'   \item{\strong{Data}}{a dataframe containing observations prior to matching}
#' }
#'
#'@seealso \code{\link[Matching]{Match}},
#'\code{\link[QEDinfR]{summary.qed_match}},
#'\code{\link[QEDinfR]{test.qed_match}}
#'
#'@examples
#'
#'data(lalonde)
#'# For matching with propensity scores:
#'ps_match <- qed_match(treat ~ age + educ + black, "re78", lalonde, "propensityscore", caliper = NULL, replace = FALSE)
#'# For matching with Mahalanobis distances:
#'md_match <- qed_match(treat ~ age + educ + black, "re78", lalonde, "mahalanobis", caliper = NULL, replace = FALSE)
#'


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

      result <- append((ps.match), list("Matched" = matched, "Call" = formula, "Distance" = "Propensity Score", "Data" = data, "Outcome" = outcome))
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

      result <- append((maha.match), list("Matched" = matched, "Call" = formula, "Distance" = "Mahalanobis", "Data" = data, "Outcome" = outcome))
      class(result) <- c("qed_match", "Match")

      invisible(result)
      }


}

