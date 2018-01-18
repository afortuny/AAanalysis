


top5_transfer_demand <-
  function(transfer_demand_table, analysis_group) {
    MAX_CASES <- 5

    top5 <- c()

    for (r in 1:nrow(transfer_demand_table)) {
      top5_cases <-
        sort(transfer_demand_table[r, 2:ncol(transfer_demand_table), drop = FALSE],
             na.last = TRUE,
             decreasing = TRUE)

      top5_cases <-
        top5_cases[, 1:min(ncol(top5_cases), MAX_CASES), drop = FALSE]
      top5_cases[is.na(top5_cases)] <- 0

      top5_temp <-
        cbind(
          rep(as.character(transfer_demand_table[r, 1]), min(ncol(top5_cases), MAX_CASES)),
          colnames(top5_cases),
          t(top5_cases[1, ]),
          1:min(ncol(top5_cases), MAX_CASES),
          rep(as.character(analysis_group), min(ncol(top5_cases), MAX_CASES))
        )

      dimnames(top5_temp) <- list(
        NULL,
        c(
          "Article_to_delete",
          "Article_to_keep",
          "Transferability_perc",
          "Ranking",
          "Analysis_group"
        )
      )

      top5 <- rbind(top5, top5_temp)
    }

    top5
  }
