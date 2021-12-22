summary.qed_match <- function(qed_match, ...){

  # Print title
  cat("Matching Summary Statistics", "\n")
  cat("---------------------------------------------------------------", "\n",
      "Formula: ",
      deparse(qed_match$call[[2]]), "\n",
      "Data: ",
      deparse(qed_match$call[[3]]), "\n",
      "Distance: ", "\n",
      "Method: Greedy Matching (Nearest Neighbor Matching)", "\n",
      "---------------------------------------------------------------", "\n", "\n",
      sep = "")

  # Pre-Matched means (by treatment/control) and Standardized Mean Difference for all covariates
  pre_matched <- data.frame()
  numeric_ticker = 0
  for(i in 1:ncol(qed_match$X)){
    df_pre <- cbind(qed_match$X, qed_match[[9]])


    if(is.numeric(df_pre[[i]]) == TRUE){
    numeric_ticker = numeric_ticker + 1
    means <- aggregate(df_pre[[i]], list(df_pre[[ncol(df_pre)]]), FUN=mean)
    sd <- aggregate(df_pre[[i]], list(df_pre[[ncol(df_pre)]]), FUN=sd)
    pre_matched <- rbind(pre_matched, data.frame('Means_for_Control' = means[1,2],
                                                 'SD_for_Control' = sd[1,2],
                                                 'Means_for_Treated' = means[2,2],
                                                 'SD_for_Treated' = sd[2,2]))
    rownames(pre_matched)[numeric_ticker] <- names(df_pre)[i]
    }

    else {
      means <- prop.table(table(df_pre[[i]], df_pre[[ncol(df_pre)]]))
      means_df <- as.data.frame.matrix(means)
      names(means_df) <- c('Means_for_Control', 'Means_for_Treated')
      rownames(means_df) <- paste0(names(df_pre)[3], "_", rownames(means_df))

      # for(j in length(levels(df[i]))){
      #   for(k in un)
      # level_binary <- ifelse((df[[i]] == levels(df[[i]])[j]), 1, 0)
      #
      #   }



      #pre_matched <- rbind(pre_matched, means_df)

    }
  }

  pre_matched

  cat("Pre-Matched Balance Summary:", "\n")
  cat("---------------------------------------------------------------", "\n")
  print(pre_matched)
  cat("---------------------------------------------------------------", "\n")
}



