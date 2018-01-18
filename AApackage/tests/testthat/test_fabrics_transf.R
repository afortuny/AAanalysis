
require(AApAcKaGe)

context("test fabrics_transf() function")

test_that('fabrics_transf test.', {

  load("data/data_group.Rda")
  data_group <- get("data_group")

  fabric <- "fabric"

  fabric_limits <- fabrics_transf(data_group, fabric = fabric)

  #test none of the fabric is na
  expect_true(length(fabric_limits[,fabric]) == length(unique(data_group[, fabric])))

})
