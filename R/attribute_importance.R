#' attribute_importance.R
#'
#'

methods::setClass("attribute_importance_item",
  slots = c(
    article = "character",
    grouping = "character",
    primary_attribute_name = "character",
    primary_attribute_value = "character",
    secondary_attribute_name = "character",
    secondary_attribute_name_grouped = "character",
    secondary_attribute_value = "character",
    rank = "integer"
  )
)

attribute_importance.item <- function(article, grouping)
  methods::new("attribute_importance_item",
    article = article,
    grouping = grouping,
    primary_attribute_name = NA_character_,
    primary_attribute_value = NA_character_,
    secondary_attribute_name = NA_character_,
    secondary_attribute_name_grouped = NA_character_,
    secondary_attribute_value = NA_character_,
    rank = NA_integer_
  )

add_row.func <- tibble::add_row

attribute_importance.append <- function(tibble, item, technical_settings) {

  f <- technical_settings$field

  x <- c()
  x <- c(x, setNames(item@article, f$article))
  x <- c(x, setNames(item@grouping, f$grouping))
  x <- c(x, setNames(item@primary_attribute_name, f$primary_attribute_name))
  x <- c(x, setNames(item@primary_attribute_value, f$primary_attribute_value))
  x <- c(x, setNames(item@secondary_attribute_name, f$secondary_attribute_name))
  x <- c(x, setNames(item@secondary_attribute_name_grouped, f$secondary_attribute_name_grouped))
  x <- c(x, setNames(item@secondary_attribute_value, f$secondary_attribute_value))
  x <- c(x, setNames(item@rank, f$rank))

  do.call("add_row.func", c(list(tibble), as.list(x)))
}

attribute_importance.new <- function(ts) {

  create.tibble <- function(...) {
    m <- matrix(ncol = length(list(...)), nrow = 0)
    colnames(m) <- list(...)
    tibble::as.tibble(m)
  }

  create.tibble(
    ts$field$article,
    ts$field$grouping,
    ts$field$primary_attribute_name,
    ts$field$primary_attribute_value,
    ts$field$secondary_attribute_name,
    ts$field$secondary_attribute_name_grouped,
    ts$field$secondary_attribute_value,
    ts$field$rank
  )
}
