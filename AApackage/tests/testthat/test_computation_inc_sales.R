require(assortment)

context("test computation_inc_sales() function")

test_that('Calculation incremental sales.', {
  load("data/raw_range_data_att.RData")

  primary_attributes <-
    c("Mat_OUTSOLE_ASSEMBLY",
      "Col_LACE_ASSEMBLY",
      "price_cat",
      "Article_description")
  Output1 <-
    computation_inc_sales(
      raw_range_data_att = raw_range_data_att,
      prim_attr_weights = c(0.2355152, 0.2222791, 0.2339549, 0.3082508),
      analysis_filters = c("ADI PERF FTW MEN RUN"),
      primary_attributes = primary_attributes
    )

  # Output 1 contains for each article the incremental sales and the sales due to each primary attribute
  # The incremental sales should be lower or equal than the actual sales

  expect_true(all(Output1[, "norm_sales"] >= Output1[, "inc_sales"]))

  #  The incremental sales cumulative sum must be 100% at the end of the file

  expect_true(Output1[nrow(Output1), "inc_sales_cum_sum_perc"] == 100)

  # the sum of the sales split per attribute must be equal to the total volume
  columns <-
    c(
      "VolMat_OUTSOLE_ASSEMBLY",
      "VolCol_LACE_ASSEMBLY",
      "Volprice_cat",
      "VolArticle_description"
    )
  expect_true(sum(Output1[, columns]) == sum(Output1[, "norm_sales"]))





})
