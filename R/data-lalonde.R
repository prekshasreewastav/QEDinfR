#' Lalonde Dataset
#'
#' This dataset was used by Deheija and Wahba (1999) to evaluate propensity score matching. It was then used by Jasjeet Sekhon in the Matching package.
#' It contains information pertaining to a range of demographic variables, real earnings, indicator for real earings being zero, and a treatment indicator,
#' and is commonly used to demonstrate causal inference and analysis. The lalonde dataset consists of 445 observations and 12 variables, including:
#' \describe{
#'   \item{\strong{age}}{age in years}
#'   \item{\strong{educ}}(number of years of education completed)
#'   \item{\strong{black}}{dummy variable to indicate if a person is black or not}
#'   \item{\strong{hisp}}{dummy variable to indicate if a person is hispanic or not}
#'   \item{\strong{married}}{dummy variable to indicate if a person is married or not}
#'   \item{\strong{nodegr}}{dummy variable to indicate if a person has attained a highschool diploma or not}
#'   \item{\strong{re74}}{real earnings in 1974}
#'   \item{\strong{re75}}{real earnings in 1975}
#'   \item{\strong{re78}}{real earnings in 1978}
#'   \item{\strong{u74}}{dummy variable to indicate whether of not real earnings in 1974 were 0 or not}
#'   \item{\strong{u75}}{dummy variable to indicate whether of not real earnings in 1975 were 0 or not}
#'   \item{\strong{treat}}{treatment indicator}
#'}
#'
#'
#' @docType data
#'
#' @usage data(lalonde)
#'
#' @format An object of class \code{rda}
#'
#' @keywords datasets
#'
#' @references Dehejia, Rajeev and Sadek Wahba. 1999.“Causal Effects in Non-Experimental Studies: Re-Evaluating the Evaluation of Training Programs.” Journal of the American Statistical Association 94 (448): 1053-1062.

#'
#' @source \link{Matching} package

"lalonde"
