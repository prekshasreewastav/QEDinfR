#' @title Generic plotting function for "qed_did" objects
#'
#' @description
#' This function serves as a generic plot method for objects of class "qed_did", a result of a call to \link[QEDinfR]{qed_did}.
#'
#' @param qed_did an object of class "qed_did"
#'
#' @import ggplot2
#'
#' @export
#'
#' @details
#' Edits can be made to the plots using ggplot functions (such as changes to axis ticks, labels, etc).
#'
#'
#' @return a ggplot object visualizing trend lines between the pre-treatment period and  the post-treatment period
#' amongst the treatment and control groups. The points signify mean values of the dependent variable pre-treatment and post-treatment.
#'
#' @examples
#'
#' data(banks)
#' did <- qed_did("num_banks_in_business", "year", 1930,  "bank_district", "6th district", df = banks)
#' plot(did)
#'

plot.qed_did <- function(qed_did, ...){
  require(ggplot2)

  #Extract dataframe from fit
  df <- qed_did[["model"]]

 #Plot trends by treatment and post
  ggplot()+
    stat_summary(data = df, aes(x = factor(post), y = df[[1]], color = factor(treatment)),
                 fun = "mean", geom="point")+
    stat_summary(data = df, aes(x = factor(post), y = df[[1]], color = factor(treatment), group = treatment),
                 fun = "mean", geom ="line") +
    scale_color_brewer("Treatment", palette = "Dark2") +
    labs(title = "Difference In Difference Trends by Treatment",
         x = "Pre-Treatment vs. Post-Treatment",
         y = paste(names(df)[1])) +
    theme_minimal()

}
