require(assortment)

context("test attribute_regression() function")

test_that('attribute_regression test.', {
  load("data/g_attribute_data.RData")
  g_attribute_data <- get("g_attribute_data")

  #Define primary attributes based on global attribute creation function
  primary_attributes = colnames(g_attribute_data[, which(!(
    colnames(g_attribute_data) %in%
      c("Article_Number", "norm_sales")
  ))])

  mockery::stub(save_path_factory, "dir.create", NA)

  save_path <- save_path_factory("1", "test")

  set.seed(123)

  mockery::stub(attribute_regression, "save", NA)
  mockery::stub(attribute_regression, "pdf", NA)
  mockery::stub(attribute_regression, "boxplot", NA)
  mockery::stub(attribute_regression, "dev.off", NA)

  # workaround for --AS-CRAN limitation: only 2 threads allowed
  mockery::stub(attribute_regression, "parallel::detectCores", 32)

  test0 <-   attribute_regression(
    data_regression = g_attribute_data,
    analysis_filters = c("W", "ADI PERF FTW WOM RUN"),
    maximum_pvalue = 0.5,
    max_weight = 0.6,
    models_until = 5,
    combi = 100,
    primary_attributes = primary_attributes,
    save_path = save_path
  )

  # Output 1 provides the attribute weights
  output1 <- test0[1]
  expect_true(sum(unlist(output1)) == 1)

})
