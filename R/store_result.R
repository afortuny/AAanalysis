#'
#'  store_result.R
#'
#'
#'
#'
#'
store_result <- function(technical_settings, spark_context, report_progress, request, processed_data) {

  insert_partition <- function(table, request_id, tibble) {

    rp <- report_progress$subarea(table)

    temp_table_name <- "assortment_temp_table"

    sparklyr::sdf_copy_to(
      spark_context,
      tibble,
      temp_table_name,
      memory = TRUE,
      overwrite = TRUE
    )

    sql <- sprintf(
      "insert into table %s partition(request_id='%s') select * from %s",
      table, request_id, temp_table_name
    )

    sparklyr::hive_context(spark_context) %>%
      sparklyr::invoke("sql", sql)

    rp$info(paste(nrow(tibble), 'records inserted'))
  }

  insert_partition(
    technical_settings$table$attribute_importance,
    request$request_id,
    processed_data$attribute_importance
  )

  insert_partition(
    technical_settings$table$transferability,
    request$request_id,
    processed_data$transferability
  )
}
