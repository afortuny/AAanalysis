



require(assortment)

context("test transferable_demand() function")



test_that('Transferable Demand test.', {
  load("data/raw_range_data_att.RData")

  mockery::stub(report_progress_factory, "print", NA)

  report_progress <- report_progress_factory()

  retain_threshold <- 0.99

  output <- transferable_demand(
    raw_range_data_att = raw_range_data_att,
    prim_attr_weights = c(0.2394654, 0.2253099, 0.2247120, 0.3105127),
    analysis_filters = c("ADI PERF FTW MEN RUN"),
    retain_threshold = retain_threshold,
    primary_attributes = c(
      "Mat_OUTSOLE_ASSEMBLY",
      "Col_LACE_ASSEMBLY",
      "price_cat",
      "Article_description"
    ),
    grouping = c("Running_man_performance"),
    report_progress =  report_progress
  )

  # top 5 gives us the top 5 report of article deletetion
  top5 <- as.data.frame(output[1])

  # transfer all give us info of all demand transfer, not only top 5 cases
  transfer_all <- as.data.frame(output[2])
  #test that sales lost relative is not higher than 100

  expect_true(max(top5[, "total_sales_lost_relative"]) < 100)

  # test that none of the articles to delete appear as article to keep


  expect_true(length(which(
    as.character(top5[, "article_to_keep"]) %in%  as.character(top5[, "article_to_delete"])
  )) == 0)



  # test that there is not article to delete, article to keep duplicates

  expect_true(nrow(subset(top5, duplicated(top5[, c("article_to_delete", "article_to_keep")]))) ==
      0)

  # test that total demand lost is lowet than threshold

  demand_lost <-
    sum(transfer_all[!duplicated(transfer_all[, "article_to_delete"]), ][, "sales_lost_absolut"])
  expect_true(demand_lost / sum(raw_range_data_att[, "norm_sales"]) < retain_threshold)

  # test that there are no duplicates

  expect_true(nrow(subset(
    transfer_all, duplicated(transfer_all[, c("article_to_delete", "article_to_keep")])
  )) ==
      0)


  # test that no article to delete is as article to keep


  expect_true(length(which(
    as.character(transfer_all[, "article_to_keep"]) %in%  as.character(transfer_all[, "article_to_delete"])
  )) == 0)


  # test that total demand of remaining articles is not higher than remaining article
  x <-
    transfer_all[!duplicated(transfer_all[, "article_to_keep"]), ][, "article_to_keep"]
  y <-
    transfer_all[!duplicated(transfer_all[, "article_to_delete"]), ][, "article_to_delete"]
  z <- c(x, y)
  total_new_demand <-
    sum(transfer_all[!duplicated(transfer_all[, "article_to_keep"]), ][, "new_total_sales_keep"])
  total_new_demand <-
    total_new_demand + sum(subset(raw_range_data_att, !raw_range_data_att[, "Article_Number"] %in% z)[, "norm_sales"])

  expect_true(total_new_demand / sum(raw_range_data_att[, "norm_sales"]) >=
      retain_threshold)

})
