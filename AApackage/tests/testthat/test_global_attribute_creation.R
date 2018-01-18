require(assortment)

context("test global_attribute_creation() function")

test_that('Global Attribute Creation test.', {
  set.seed(123)

  load("data/data_mart_assort_ARP_clean.RData")
  data_mart_assort_ARP_clean <- get("data_mart_assort_ARP_clean")

  mockery::stub(save_path_factory, "dir.create", NA)

  save_path <- save_path_factory("1", "test")

  #test1 mannual mode
  #define the vector of primary attributes to analyze, excluding price
  primary_attributes <- colnames(data_mart_assort_ARP_clean)[c(11:62)]

  mockery::stub(global_attribute_creation, "save", NA)
  mockery::stub(global_attribute_creation, "pdf", NA)
  mockery::stub(global_attribute_creation, "ggplot2::ggplot", NA)
  mockery::stub(global_attribute_creation, "ggplot2::geom_vline", NA)
  mockery::stub(global_attribute_creation, "ggplot2::ggtitle", NA)
  mockery::stub(global_attribute_creation, "ggplot2::geom_segment", NA)
  mockery::stub(global_attribute_creation, "ggplot2::geom_point", NA)
  mockery::stub(global_attribute_creation, "ggplot2::theme_bw", NA)
  mockery::stub(global_attribute_creation, "ggplot2::theme", NA)
  mockery::stub(global_attribute_creation, "print", NA)
  mockery::stub(global_attribute_creation, "dev.off", NA)

  # run the function for manually populated price segments
  test1 <-
    global_attribute_creation(
      range_data_attrib = data_mart_assort_ARP_clean,
      primary_attributes = primary_attributes,
      #                             man_price_seg=c(0,60,80,100,120,500),
      #                             price_cluster_criteria="Manual",
      atributte_levels = 6,
      atributte_theshold = 5,
      analysis_filters = c("M", "ADI PERF FTW MEN RUN"),
      save_path = save_path
    )

  test1 <- test1[[1]]

  # Testing 1
  # expect_equal(dim(test1), c(84,6))
  expect_equal(round(sum(test1$norm_sales), 0), 63410)
  # expect_equal(length(levels(test1$Mat_OUTSOLE_ASSEMBLY)),7)
  # expect_equal(length(levels(test1$Col_LACE_ASSEMBLY)),7)
  # expect_equal(length(levels(test1$price_cat)),5)
  # expect_equal(length(levels(test1$Article_description)),7)
  expect_equal(length(unique(test1$Article_Number)), 84)

  #test2 auto mode
  test2 <-
    global_attribute_creation(
      range_data_attrib = data_mart_assort_ARP_clean,
      primary_attributes = primary_attributes,
      #                             price_cluster_number=5,
      #                             price_cluster_criteria="Automatic",
      atributte_levels = 6,
      atributte_theshold = 5,
      analysis_filters = c("M", "ADI PERF FTW MEN RUN"),
      save_path = save_path
    )

  test2 <- test2[[1]]

  # Testing 2
  # expect_equal(dim(test2), c(84,6))
  expect_equal(round(sum(test2$norm_sales), 0), 63410)
  # expect_equal(length(levels(test2$Mat_OUTSOLE_ASSEMBLY)),7)
  # expect_equal(length(levels(test2$Col_LACE_ASSEMBLY)),7)
  # expect_equal(levels(test2$price_cat)[5],"(81,110]")
  # expect_equal(length(levels(test2$Article_description)),7)
  expect_equal(length(unique(test2$Article_Number)), 84)

})
