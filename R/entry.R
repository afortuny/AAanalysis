#'
#' entry.R
#'
#' Assortment Analysis entry point
#' Expects pre-loaded (sourced) technical and analytical settings functions
#'
#' @param technical_settings environment (dev/test/prod) configuration
#' @param analytical_settings analysis parameters
#'
#' @return none
#'
#' @author Vadim Zayakin <vadims.zajakins@@accenture.com>
#'
#' All imports are mentioned here to avoid repeated import definitions in each source file
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom foreach %dopar%
#'
#' @importFrom stats aggregate
#' @importFrom stats as.formula
#' @importFrom stats complete.cases
#' @importFrom stats kmeans
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats reshape
#' @importFrom stats setNames
#'
#' @importFrom grDevices pdf
#' @importFrom grDevices dev.off
#'
#' @importFrom graphics barplot
#' @importFrom graphics boxplot
#' @importFrom graphics legend
#' @importFrom graphics par
#'
entry.assortment <- function(technical_settings, analytical_settings) {

  spark_context <- technical_settings$spark_connect()

  process_assortment(technical_settings, analytical_settings, spark_context)

  technical_settings$spark_disconnect()
}
