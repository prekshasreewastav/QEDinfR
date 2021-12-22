#'@title qed_rdd: Regression Discontinuity Design
#'
#'@description
#'
#'Estimates a simple Regression Discontinuity Design model with convenient methods
#'for summary and plotting
#'
#'@param y string, name of depedent variable/outcome
#'@param x string, name of independent variable
#'@param cutoff_criterion number, indicates where the discontinuity in x occurs
#'@param df A dataframe or tibble
#'@param transform boolean, whether or not to substract cutoff criterion from \code{x}
#'@param interaction boolean, whether or not to include an interaction between treatment and the covariate \code{x}
#'
#'
#'@export
#'
#'@details
#'text
#'
#'@seealso \code{\link[stats]{lm}} \code{\link[QEDinfR]{plot.qed_rdd}}
#'
#'
#'@return an object of type \code{qed_rdd}
#'
#'@examples
#'\dontrun{
#'data(mlda)
#'fit <- qed_rdd("all", "agecell", 21, mlda)
#'summary(fit)
#'}

qed_rdd <- function(y, x, cutoff_criterion, df, transform = TRUE, interaction = TRUE){

   # calculate the treatment dummy variable based off of cutoff criterion
   df[, "treated"] <- df[ , x] >= cutoff_criterion

  # if transform TRUE, transform x var such that 0 is the cutoff
   if (transform){
    df[ , x] = df[ , x] - cutoff_criterion
  }


  if (interaction){
    formula <- as.formula(paste(y,
                                paste(x, "treated", sep = "*"),
                                sep = "~"))
  }
  else {
    formula <- as.formula(paste(y,
                                paste(x, "treated", sep = "+"),
                                sep = "~"))
  }


  fit <- lm(formula, df)

  class(fit) <- c("qed_rdd", "lm")

  fit$call <- str2lang(paste("lm(formula=", deparse(substitute(formula)),
                             ", data=", deparse(substitute(data)), ")"))

  return(fit)
}


