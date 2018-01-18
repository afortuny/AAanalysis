#'
#' load_data.R
#'
#' @param  technical_settings   Environment (development/testing/production) parameters
#' @param  analytical_settings  Analysis customization anf fine tuning parameters
#' @param  spark_context        The Spark connection
#' @param  report_progress      Logging facility
#' @param  request              Assortment request to process
#'
#' @return Sales data and attributes
#'
#' @author Vadim Zayakin <vadims.zajakins@@accenture.com>
#'
load_data <- function(technical_settings, analytical_settings, spark_context, report_progress, request) {

    ts <- technical_settings

    read.by.request <- function(table.name, ...)
      spark_context %>%
        dplyr::tbl(table.name) %>%
        dplyr::filter(rlang::sym(!! ts$field$request) == !! request[[ts$field$request]]) %>%
        dplyr::select(...)

    scope <- read.by.request(
      table.name = ts$table$scope,
      ts$field$article, ts$field$grouping
    )

    sales <- read.by.request(
      table.name = ts$table$sales,
      ts$field$article, ts$field$grouping, ts$field$sales, ts$field$price
    )

    attrs_long <- read.by.request(
      table.name = ts$table$attributes,
      ts$field$article, ts$field$grouping, ts$field$key, ts$field$value
    )

    attrs <- tidyr::spread(
      dplyr::collect(attrs_long),
      key = ts$field$key,
      value = ts$field$value
    )

    data <-
      scope %>%
        dplyr::inner_join(sales, by = c(ts$field$article, ts$field$grouping)) %>%
        dplyr::collect() %>%
        dplyr::left_join(attrs, by = c(ts$field$article, ts$field$grouping))

    articles <-
      scope %>%
        dplyr::select(!! ts$field$article) %>%
        dplyr::distinct()

    if (request[[ts$field$prod_division]] == 'FTW') {

      pdna <-
        footwear_pdna_data_preparation(
          analytical_settings,
          technical_settings,
          spark_context,
          articles
        )

      data <-
        data %>%
          dplyr::left_join(
            dplyr::rename(
              pdna,
              !! ts$field$article := "Article"
            ),
            by = ts$field$article
          )
    }

    if (request[[ts$field$prod_division]] == 'APP') {

      #FIXME: apparel-specific BDP attributes may be loaded here...
    }

    #FIXME: current code uses hardcoded field names - keep for compatibility...
    #(may be removed after remaining of the code is refactored to use configurable fields)
    data <-
      data %>%
        dplyr::rename(
          'Grouping'       := !! ts$field$grouping,
          'Article_Number' := !! ts$field$article,
          'ARP'            := !! ts$field$price,
          'norm_sales'     := !! ts$field$sales
        )

    v_article_full <-
      spark_context %>%
        dplyr::tbl(dplyr::sql(analytical_settings$sql_txt_v_article_rfd)) %>%
        dplyr::rename(!! ts$field$article := "article_no") %>%
        dplyr::inner_join(articles, by = ts$field$article) %>%
        dplyr::collect()

    list(
      'data' = as.data.frame(data),
      'v_article_full' = as.data.frame(v_article_full)
    )
  }
