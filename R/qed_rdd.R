#'@title qed_rdd: Regression Discontinuity Design
#'
#'@description
#'
#'Estimates a simple Regression Discontinuity Design model with convenient methods
#'for summary and plotting
#'
#'@param y string, name of dependent variable/outcome
#'@param x string, name of independent variable
#'@param cutoff_criterion numeric, indicates where the discontinuity in x occurs
#'@param df dataframe or tibble
#'@param transform logical, whether or not to subtract cutoff criterion from
#'@param interaction logical, whether or not to include an interaction between treatment and the covariate
#'
#'
#'@export
#'
#'@details
#'\code{\link[QEDinfR]{qed_rdd}} estimates basic regression discontinuity designs and offers a convenient plotting method to display the discontinuous relationship.
#'The function automatically transforms the discontinuous covariate to center the discontinuity at zero.
#'By default it allows for an interaction effect.
#'
#'@seealso \code{\link[stats]{lm}} \code{\link[QEDinfR]{plot.qed_rdd}}
#'
#'
#'@return an object of type \code{\link[QEDinfR]{qed_rdd}}
#'
#'@examples
#'data(mlda)
#'fit <- qed_rdd("all", "agecell", 21, mlda)
#'summary(fit)
#'

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


