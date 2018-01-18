
context("test transferability class")

test_that('transferability class - append',
  {
    source(system.file("config/technical_settings.R", package = "assortment"))

    mockery::stub(get_technical_settings, "sparklyr::spark_config", "<<spark_config>>")

    ts <- get_technical_settings()

    tibble <- transferability.new(ts = ts)

    item <- transferability.item("AB1234", "grouping-01")

    item@article_to_keep <- "to-keep"
    item@article_to_delete <- "to-delete"
    item@transferability_percentage <- 25.9
    item@absolute_sales_transferred <- 123.0
    item@ranking_to_delete <- 10L
    item@new_total_sales_receiving <- 8.8
    item@sales_to_delete <- 0.009
    item@total_sales_transferability <- 100.00

    tibble <- transferability.append(tibble, item, ts)

    item <- transferability.item("AB5678", "grouping-02")

    item@article_to_keep <- "to-keep"
    item@article_to_delete <- "to-delete"
    item@transferability_percentage <- 99.9

    tibble <- transferability.append(tibble, item, ts)

    testthat::expect_equal(nrow(tibble), 2)
    testthat::expect_equal(ncol(tibble), 12)
  }
)

test_that('transferability class - incorrect slot',
  {

    item <- transferability.item("AB1234", "grouping-01")

    testthat::expect_error(item@incorrect <- "some text")
  }
)

test_that('transferability class - invalid type in slot',
  {

    item <- transferability.item("AB1234", "grouping-01")

    testthat::expect_error(item@absolute_sales_transferred <- "something")
    testthat::expect_error(item@article_to_keep <- TRUE)
  }
)


