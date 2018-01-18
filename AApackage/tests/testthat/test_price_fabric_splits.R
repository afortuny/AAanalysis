
require(assortment)

context("test price_fabric_splits() function")

test_that('price fabric split test.', {

  load("data/data_group.Rda")
  data_group <- get("data_group")

  area <- 'Apparel'

  # Report code progress

  nrow_data_group <- nrow(data_group)

  mockery::stub(report_progress_factory, "print", NA)
  report_progress <- report_progress_factory(area)

  fabric <- "fabric"

  suppressWarnings(
    data_group <-
      price_fabric_splits(
        data_group = data_group,
        report_progress = report_progress)
  )

  #test none of the fabric is na

  expect_true(!any(is.na(data_group[,fabric])))

  #test none of the fabric group is na

  expect_true(!any(is.na(data_group[,fabric])))

  #test none of the price group is na

  expect_true(!any(is.na(data_group[,"price_cat"])))

  #test no articles are droped

  expect_true(nrow(data_group) == nrow_data_group)

})
