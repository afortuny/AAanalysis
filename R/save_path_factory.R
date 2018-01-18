#' save_path_factory
#'
#' Creates the required paths to store the local output files
#'
#' @param session_id A unique identifier for the run of the code
#' @param prod_division A string identifying apparel and footwear
#'
#' @return The list of functions returning full file path to store analysis outputs
#'
#' @author Zajakins, Vadims <vadims.zajakins@accenture.com>
#' @author Fortuny Sicart, Alan <Alan.FortunySicart@adidas-group.com>
#'
save_path_factory <- function(session_id, prod_division) {
  any_path_with_prefix <- function(prefix)
    paste0("output", "/", session_id, "/", prefix, "/", prod_division)

  encode <- function(x)
    gsub(' ', '_',
      gsub('/', '_', x))

  any_path_with_suffixes <- function(prefix, suffixes)
    paste0(any_path_with_prefix(prefix),
           "_",
           encode(paste0(suffixes, collapse = "_")))

  build <- function(path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    paste0(path, "/")
  }

  list(
    ds_file = function(suffixes, basename, ext)
      paste0(build(any_path_with_suffixes("data_scientist", suffixes)), encode(basename), ext),

    pd_file = function(suffixes, basename, ext)
      paste0(build(any_path_with_suffixes("product_designer", suffixes)), encode(basename), ext),

    mm_file = function(suffixes, basename, ext)
      paste0(build(any_path_with_suffixes("merchandise_manager", suffixes)), encode(basename), ext),

    ts_file = function(suffixes, basename, ext)
      paste0(build(any_path_with_suffixes("transferability", suffixes)), encode(basename), ext),

    top_file = function(basename, ext)
      paste0(build(any_path_with_prefix("top")), encode(basename), ext)
  )
}
