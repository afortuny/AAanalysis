#'
#' submit_assortment_requests.R
#'

get_next_request_id <- function()
  format(
    as.POSIXlt(Sys.time(), tz = "utc") ,
    "%Y%m%d.%H%M%OS6"
  )

create.request <- function(season, market_id, prod_division, comment = ' ')
  dplyr::as_tibble(
    data.frame(
      season = season,
      market_id = market_id,
      prod_division = prod_division,
      comment = comment
    )
  )

insert_partition <- function(sc, rp, table, request_id, tibble) {

  rp <- rp$subarea(table)

  temp_table_name <- "assortment_temp_table"

  sparklyr::sdf_copy_to(
    sc,
    tibble,
    temp_table_name,
    memory = TRUE,
    overwrite = TRUE
  )

  sql <- sprintf(
    "insert into table %s partition(request_id='%s') select * from %s",
    table, request_id, temp_table_name
  )

  sparklyr::hive_context(sc) %>%
    sparklyr::invoke("sql", sql)

  rp$info(paste(nrow(tibble), 'records inserted'))
}


submit_single_request <- function(ts, sc, rp, request_id, request, articles, sales, attributes) {

  rp <- rp$subarea(request_id, key = "request")

  insert_partition(sc, rp, ts$table$request, request_id, request)

  scope <-
    sales %>%
      dplyr::inner_join(articles, by = ts$field$article) %>%
      dplyr::select(ts$field$article, ts$field$grouping)

  insert_partition(sc, rp, ts$table$scope, request_id, scope)

  insert_partition(sc, rp, ts$table$sales, request_id, sales)

  attributes_long <-
    attributes %>%
      tidyr::gather(
        key = !! ts$field$key,
        value = !! ts$field$value,
        -c(!! ts$field$article, !! ts$field$grouping)
      )

  insert_partition(sc, rp, ts$table$attributes, request_id, attributes_long)

  status <- dplyr::as_tibble(
    data.frame(
      description = ' ',
      status = STATUS_SUBMITTED
    )
  )
  insert_partition(sc, rp, ts$table$status, request_id, status)
}

submit_assortment_requests <- function(technical_settings, analytical_settings, spark_context) {

  ts <- technical_settings
  as <- analytical_settings
  sc <- spark_context

  rp <- report_progress_factory("Apparel")

  data <- tibble::as.tibble(
    utils::read.csv2(
      as$table_sales_attributes,
      sep = ',',
      dec = '.',
      stringsAsFactors = FALSE
    )
  )

  rp$info(paste(nrow(data), "lines read from", as$table_sales_attributes))

  data <-
    data %>%
      dplyr::rename(
        !! ts$field$article  := !! as$article_id_name_sales,
        !! ts$field$grouping := "Groupings",
        !! ts$field$sales    := !! as$sales_name_ap,
        !! ts$field$price    := !! as$price_name_ap
      )

  articles <-
    data %>%
      dplyr::select(ts$field$article) %>%
      dplyr::distinct()

  attributes <-
    data %>%
      dplyr::select(-dplyr::one_of(ts$field$sales, ts$field$price))

  sales <-
    data %>%
      dplyr::select(ts$field$article, ts$field$grouping, ts$field$sales, ts$field$price)

  submit_single_request(ts, sc, rp,
    get_next_request_id(),
    create.request("FW", "NAM", "APP"),
    articles,
    sales,
    attributes
  )

  rp <- report_progress_factory("Footwear")

  data <- tibble::as.tibble(
    utils::read.csv2(
      as$table_sales,
      sep = ',',
      dec = '.',
      stringsAsFactors = FALSE
    )
  )

  rp$info(paste(nrow(data), "lines read from", as$table_sales))

  data <-
    data %>%
      dplyr::rename(
        !! ts$field$article  := !! as$article_id_name_sales,
        !! ts$field$grouping := !! as$Grouping,
        !! ts$field$sales    := !! as$sales_name_fw,
        !! ts$field$price    := !! as$price_name_fw
      )

  load(file = as$table_articles_in_scope)
  articles <-
    data.frame(articles_ftwr) %>%
      dplyr::select(!! ts$field$article := "Article") %>%
      dplyr::distinct() %>%
      tibble::as.tibble()
  rm(articles_ftwr)

  rp$info(paste(nrow(data), "records read from", as$table_articles_in_scope))

  attributes <-
    data %>%
      dplyr::select(-dplyr::one_of(ts$field$sales, ts$field$price))

  sales <-
    data %>%
      dplyr::select(ts$field$article, ts$field$grouping, ts$field$sales, ts$field$price)

  submit_single_request(ts, sc, rp,
    get_next_request_id(),
    create.request("FW", "NAM", "FTW"),
    articles,
    sales,
    attributes
  )

}
