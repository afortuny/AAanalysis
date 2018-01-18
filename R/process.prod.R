#'
#' process.prod.R
#'
#' Assortment Analysis request pprocessing entry point for production environment
#'
#' @author Vadim Zayakin <vadims.zajakins@@accenture.com>
#'

devtools::load_all()
source(system.file("config/technical_settings.R", package = "assortment"))
source(system.file("config/analytical_settings.R", package = "assortment"))

# debug(process_assortment_requests)
entry(process_assortment_requests)
