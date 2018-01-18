require(assortment)

context("test attribute_analysis() function")

test_that('Attribute Analysis test.', {
  load("data/models_final.RData")
  load("data/g_attribute_data.RData")

  #pre-settings
  primary_attributes = colnames(g_attribute_data[, which(!(
    colnames(g_attribute_data) %in% c("Article_Number", "norm_sales")
  ))])
  prim_attr_weights = c(0.2355152, 0.2222791, 0.2339549, 0.3082508)
  secondary_attr = names(models_final[(5 + length(prim_attr_weights) +
      1):ncol(models_final)])

  save_path <- save_path_factory("1", "test")

  mockery::stub(base::save, "base::save", function(...)
    NA)

  #Run the function
  test0 <- attribute_analysis(
    models_final = models_final,
    Attribute_data_mapping = g_attribute_data,
    analysis_filters = c("M", "ADI PERF FTW MEN RUN"),
    primary_attributes = primary_attributes,
    secondary_attr = secondary_attr,
    prim_attr_weights = prim_attr_weights,
    save_path = save_path
  )

  # Testing if the output tables are consistent with the inputs
  output1 <- test0[[1]]

  expect_equal(dim(output1)[1], length(primary_attributes))
  expect_equal(round(sum(as.numeric(
    as.character(output1$Importance)
  )), 0), 100)

  output2 <- test0[[2]]
  expect_equal(dim(output2)[1], length(secondary_attr))
  expect_equal(round(sum(as.numeric(
    as.character(output2$Importance)
  )), 0), 100)


})
