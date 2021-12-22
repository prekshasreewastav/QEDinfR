#' @title qed_did: Difference in Difference Designs
#'
#' @description
#' code{QED_did} estimates basic difference in difference designs and provides a convenient plotting utility to visualize counter factual outcomes following the parallel trends assumption.
#'
#' @param y string, name of the outcome variable in dataframe
#' @param x string, name of the time variable
#' @param event_time numeric, when the event occurred
#' @param treated_id string, name of the identifying variable in \code{df}
#' @param treated vector of the same type as the identifier variable, indicates which identifiers were treated by the event
#' @param df a dataframe or tibble
#'
#'@import dplyr
#'
#' @export
#'
#' @details
#' The function automatically generates the post-treatment and treated indicators for estimating the model, and one may generate plots with \code{QED_did} objects easily with its plot method.
#' The \code{summary.lm} method provides the appropriate summary statistics.
#'
#'
#' @seealso \code{\link[stats]{lm}}
#'
#' @return object of type \code{qed_did}
#'
#'
#'@examples
#'\dontrun{
#'data(banks)
#'fit <- qed_did("num_banks_in_business", "year", 1930,  "bank_district", "6th district", df = banks)
#'summary(fit)
#'}

qed_did <- function(y, x, event_time, treated_id, treated, df){
  require(dplyr)
  df[, "treatment"] <- ifelse(df[,treated_id] == treated, 1, 0)
  df[, "post"] <- ifelse(df[,x] == event_time+1, 1, 0)

  formula <- as.formula(paste(y,
                              paste("treatment", "post", sep = "*"),
                              sep = "~"))

  fit <- lm(formula = formula, data = df)

  class(fit) <- c("qed_did", "lm")

  fit$call <- str2lang(paste("lm(formula=", deparse(substitute(formula)),
                             ", data=", deparse(substitute(data)), ")"))

  return(fit)


}

