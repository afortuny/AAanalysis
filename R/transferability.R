#' transferability.R
#'
#'
#'
#'
#'

methods::setClass("transferability_item",
  slots = c(
    article = "character",
    grouping = "character",
    article_to_keep = "character",
    article_to_delete = "character",
    transferability_percentage = "numeric",
    ranking_to_delete = "integer",
    absolute_sales_transferred = "numeric",
    new_total_sales_receiving = "numeric",
    sales_to_delete = "numeric",
    total_sales_transferability = "numeric",
    sales_lost_relative = "numeric",
    sales_lost_absolute = "numeric"
  )
)

transferability.item <- function(article, grouping)
  methods::new("transferability_item",
    article = article,
    grouping = grouping,
    article_to_keep = NA_character_,
    article_to_delete = article,
    transferability_percentage = NA_real_,
    ranking_to_delete = NA_integer_,
    absolute_sales_transferred = NA_real_,
    new_total_sales_receiving = NA_real_,
    sales_to_delete = NA_real_,
    total_sales_transferability = NA_real_,
    sales_lost_relative = NA_real_,
    sales_lost_absolute = NA_real_
  )

add_row.func <- tibble::add_row

transferability.append <- function(tibble, item, technical_settings) {

  f <- technical_settings$field

  x <- c()
  x <- c(x, setNames(item@article, f$article))
  x <- c(x, setNames(item@grouping, f$grouping))
  x <- c(x, setNames(item@article_to_keep, f$article_to_keep))
  x <- c(x, setNames(item@article_to_delete, f$article_to_delete))
  x <- c(x, setNames(item@transferability_percentage, f$transferability_percentage))
  x <- c(x, setNames(item@ranking_to_delete, f$ranking_to_delete))
  x <- c(x, setNames(item@absolute_sales_transferred, f$absolute_sales_transferred))
  x <- c(x, setNames(item@new_total_sales_receiving, f$new_total_sales_receiving))
  x <- c(x, setNames(item@sales_to_delete, f$sales_to_delete))
  x <- c(x, setNames(item@total_sales_transferability, f$total_sales_transferability))
  x <- c(x, setNames(item@sales_lost_relative, f$sales_lost_relative))
  x <- c(x, setNames(item@sales_lost_absolute, f$sales_lost_absolute))

  do.call("add_row.func", c(list(tibble), as.list(x)))
}

transferability.new <- function(ts) {

  create.tibble <- function(...) {
    m <- matrix(ncol = length(list(...)), nrow = 0)
    colnames(m) <- list(...)
    tibble::as.tibble(m)
  }

  create.tibble(
    ts$field$article,
    ts$field$grouping,
    ts$field$article_to_keep,
    ts$field$article_to_delete,
    ts$field$transferability_percentage,
    ts$field$ranking_to_delete,
    ts$field$absolute_sales_transferred,
    ts$field$new_total_sales_receiving,
    ts$field$sales_to_delete,
    ts$field$total_sales_transferability,
    ts$field$sales_lost_relative,
    ts$field$sales_lost_absolute
  )
}
