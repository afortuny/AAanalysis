require(assortment)

context("test attribute_guidance() function")

test_that('Attribute Guidance test.', {
  load("data/data_group.Rda")
  data_group <- get("data_group")


  primary_attributes <- colnames(data_group)[5:34]
  group <- "DtC_PANTS (1/1)_ADULT_MEN"


  attribute_guidance <-
    as.data.frame(
      attribute_guidance(
        data_group = data_group ,
        primary_attributes = primary_attributes,
        group = group
      )[[1]]
    )

  # confirm that we have data for all articles
  expect_true(length(unique(attribute_guidance[, "Article_Number"])) == nrow(data_group))

  # confirm that we have 100% of primary attribute weight - note that we disregard cases with less than 1% of contribution
  prim_attribute_val <-
    as.data.frame(unique(attribute_guidance[, c("primary_attribute_name", "primary_attribute_perc")]))

  expect_true(sum(prim_attribute_val[, "primary_attribute_perc"]) == 100)

})
