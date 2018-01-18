
context("test attribute_importance class")

test_that('attribute_importance class - append',
  {
    source(system.file("config/technical_settings.R", package = "assortment"))

    mockery::stub(get_technical_settings, "sparklyr::spark_config", "<<spark_config>>")

    ts <- get_technical_settings()

    tibble <- attribute_importance.new(ts = ts)

    item <- attribute_importance.item("AB1234", "grouping-01")

    item@primary_attribute_name <- "primary"
    item@primary_attribute_value <- 23.45
    item@secondary_attribute_name <- "sec"
    item@secondary_attribute_name_grouped <- "sg"
    item@secondary_attribute_value <- 12.34
    item@rank <- 1L

    tibble <- attribute_importance.append(tibble, item, ts)

    item <- attribute_importance.item("AB5678", "grouping-02")

    item@primary_attribute_name <- "primary2"
    item@primary_attribute_value <- 67.89
    item@secondary_attribute_name <- "sec2"
    item@secondary_attribute_name_grouped <- "sg2"
    item@secondary_attribute_value <- 11.11
    item@rank <- 2L

    tibble <- attribute_importance.append(tibble, item, ts)

    testthat::expect_equal(nrow(tibble), 2)
    testthat::expect_equal(ncol(tibble), 8)
  }
)

test_that('attribute_importance class - article & grouping',
  {

    item <- attribute_importance.item("article", "grouping")

    testthat::expect_equal(item@article, "article")
    testthat::expect_equal(item@grouping, "grouping")
  }
)

test_that('attribute_importance class - incorrect slot',
  {

    item <- attribute_importance.item("AB1234", "grouping-01")

    testthat::expect_error(item@incorrect <- "some text")
  }
)

test_that('attribute_importance class - invalid type in slot',
  {

    item <- attribute_importance.item("AB1234", "grouping-01")

    testthat::expect_error(item@secondary_attribute_value <- "12.2")
    testthat::expect_error(item@secondary_attribute_value <- TRUE)
  }
)


