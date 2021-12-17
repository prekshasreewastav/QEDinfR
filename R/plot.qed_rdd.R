#' @title Plotting Utilities for QED_RDD objects
#'
#' @description
#' provides convenient plot method for qed_rdd objects
#'
#' @param df object of type \code{qed_rdd}
#' @param cutoff numeric, the cutoff criterion; default is 0 as qed_rdd will by default transform
#' covariate \code{x} to \code{(x - cutoff_score)}
#' @param interaction boolean, whether or not to allow for slope to be different between treatment and control
#'
#'@import ggplot2
#'
#' @export
#'
#' @details
#' One can add and edit plot using the ggplot functions, such as using xlab or
#' ylab to change the x and y labels.
#'
#' The function automatically plots a line at the discontinuity if you kept
#' the \code{qed_rdd} default of transforming the \code{x} variable to
#' center the discontinuity at zero; otherwise, you must specify the discontinuity yourself.
#'
#' By default the plot allows for an interaction effect between \code{x} and the
#' treatment; one can however set this to false.
#'
#' @return a ggplot object
#'
#' @examples
#' \dontrun{
#' data(mlda)
#' fit <- qed_rdd("mva", "agecell", 21, mlda)
#' plot(fit)
#' plot(fit, interaction = FALSE)
#' }

plot.qed_rdd <- function(rdd_ls, cutoff = 0, interaction = TRUE){
  require(ggplot2)

  df <- rdd_ls[["model"]]

  if (interaction){
    rdd_p <- ggplot(data = NULL,
                    aes(x = df[[2]],
                        y = df[[1]],
                        color = df[[3]]))
  }
  else {
    rdd_p <- ggplot(data = NULL,
                    aes(x = df[[2]],
                        y = df[[1]]))
  }

   rdd_p +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_brewer(palette = "Pastel1") +
    guides(color = "none") +
    geom_vline(xintercept = cutoff, color = "red",
               size = 1, linetype = "dashed") +
    xlab("X") +
     ylab("Y") +
     ggtitle("Regression Discontinuity Plot")

}

