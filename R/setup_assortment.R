#'
#' setup_assortment.R
#'
#' Assortment Analysis setup
#'
#' Creates required tables or validates existing table structure.
#' Note: this code does not try to adjust table structure if existing table
#' structure is not correct.
#'
#' @param technical_settings environment (dev/test/prod) configuration
#' @param analytical_settings analysis parameters
#' @param spark_context spark connection
#'
#' @return none
#'
#' @author Vadim Zayakin <vadims.zajakins@@accenture.com>
#'
#' @export
#'
setup_assortment <- function(technical_settings, analytical_settings, spark_context) {
  ts <- technical_settings

  as.field.spec <- function(fields)
    paste(
      lapply(fields, '[[', 'name'),
      lapply(fields, '[[', 'type'),
      collapse = ',')

  create.table <- function(table.name, table.fields, table.partitions) {

    spark_table <- NULL
    try(spark_table <- dplyr::tbl(spark_context, table.name), silent = TRUE)

    if (!is.null(spark_table)) {

      act.table <- sparklyr::sdf_schema(spark_table)

      get.type <- function(x)
        switch(x$type,
          'StringType' = 'string',
          'DecimalType(12,2)' = 'decimal(12,2)',
          x$type
        )

      act.fields <- data.frame(
        'col_name' = sapply(act.table, '[[', 'name'),
        'data_type' = sapply(act.table, get.type),
        stringsAsFactors = FALSE
      )

      req.fields <- data.frame(
        'col_name' = sapply(c(table.fields, table.partitions), '[[', 'name'),
        'data_type' = sapply(c(table.fields, table.partitions), '[[', 'type'),
        stringsAsFactors = FALSE
      )

      missing <- dplyr::anti_join(req.fields, act.fields, by = c("col_name", "data_type"))
      useless <- dplyr::anti_join(act.fields, req.fields, by = c("col_name", "data_type"))

      if (nrow(missing) > 0) {
        print(paste('Table', table.name, 'missing fields:'))
        print(missing)
      }

      if (nrow(useless) > 0) {
        print(paste('Table', table.name, 'useless fields:'))
        print(useless)
      }

      if (nrow(missing) == 0 && nrow(useless) == 0)
        print(paste('Table', table.name, 'has correct structure'))
    }
    else {
      sql <- paste(
        "create table",
        table.name,
        "(",
        as.field.spec(table.fields),
        ")",
        "partitioned by",
        "(",
        as.field.spec(table.partitions),
        ")"
      )

      sparklyr::hive_context(spark_context) %>%
        sparklyr::invoke("sql", sql)

      print(paste('Table', table.name, 'is created'))
    }
  }

  field <- function(name, type = 'string')
    list('name' = name, 'type' = type)

  # ---- drop existing tables ----

  drop.table <- function(table.name) {
    sql <- paste(
      "drop table",
      table.name
    )

    print(sql)

    sparklyr::hive_context(spark_context) %>%
      sparklyr::invoke("sql", sql)
  }

  # uncomment this line to drop existing tables
# if (FALSE)
    lapply(ts$table, drop.table)

  # ---- request table ----
  create.table(
    ts$table$request,

    list(
      field(ts$field$season),             # 'FW' | 'SS'
      field(ts$field$market),             # 'CIS' | 'WE' | 'NAM' | 'CHI'
      field(ts$field$prod_division),      # 'APP' | 'FTW'
      field(ts$field$comment)
    ),

    list(field(ts$field$request))
  )

  # ---- status table ----
  create.table(
    ts$table$status,

    list(
      field(ts$field$description),
      field(ts$field$status)
    ),

    list(field(ts$field$request))
  )

  # ---- status history table ----
  create.table(
    ts$table$history,

    list(
      field(ts$field$description),
      field(ts$field$status),
      field(ts$field$log),
      field(ts$field$timestamp)
    ),

    list(field(ts$field$request))
  )

  # ---- scope table ----
  create.table(
    ts$table$scope,

    list(
      field(ts$field$article),
      field(ts$field$grouping)
    ),

    list(field(ts$field$request))
  )

  # ---- sales table ----
  create.table(
    ts$table$sales,

    list(
      field(ts$field$article),
      field(ts$field$grouping),
      field(ts$field$sales, "decimal(12,2)"),
      field(ts$field$price, "decimal(12,2)")
    ),

    list(field(ts$field$request))
  )

  # ---- attribute table ----
  create.table(
    ts$table$attributes,

    list(
      field(ts$field$article),
      field(ts$field$grouping),
      field(ts$field$key),
      field(ts$field$value, "string")
    ),

    list(field(ts$field$request))
  )

  # ---- attribute_importance table ----
  create.table(
    ts$table$attribute_importance,

    list(
      field(ts$field$article),
      field(ts$field$grouping),

      field(ts$field$primary_attribute_name),
      field(ts$field$primary_attribute_value),
      field(ts$field$secondary_attribute_name),
      field(ts$field$secondary_attribute_name_grouped),
      field(ts$field$secondary_attribute_value),
      field(ts$field$rank)
    ),

    list(field(ts$field$request))
  )

  # ---- transferability table ----
  create.table(
    ts$table$transferability,

    list(
      field(ts$field$article),
      field(ts$field$grouping),

      field(ts$field$article_to_keep),
      field(ts$field$article_to_delete),
      field(ts$field$transferability_percentage),
      field(ts$field$ranking_to_delete),
      field(ts$field$absolute_sales_transferred),
      field(ts$field$new_total_sales_receiving),
      field(ts$field$sales_to_delete),
      field(ts$field$total_sales_transferability),
      field(ts$field$sales_lost_relative),
      field(ts$field$sales_lost_absolute)
    ),

    list(field(ts$field$request))
  )

}
