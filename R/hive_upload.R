#' create Hive table with one partition
#'
#' creates Hive table if not exists
#'
#' @param sc A spark connection
#' @param name Table name. Should contain database and table name. example:
#' "some_database.my_table"
#' @param columns Table schema. Passing all coumns as string.
#' @param partitionBy Partition column
#' @examples
#' \dontrun{
#' createHiveTable(
#'   name = lab.my_table,
#'   columns = c(col1, col2, col3),
#'   partitionBy = "id"
#' )
#' }

createHiveTable <- function(sc, name, columns, partitionBy) {
  create_table_sql <- paste(
    "create table if not exists",
    name,
    "(",
    paste(columns, "string", collapse = ", "),
    ")",
    "partitioned by (",
    paste(partitionBy, "string", collapse = ", "),
    ")"
  )

  sparklyr::hive_context(sc) %>%
    sparklyr::invoke("sql", create_table_sql)
}


#' insert into Hive table
#'
#' Inserts data from Spark data frame to Hive table with partition
#'
#' @param sc A spark connection
#' @param spark_df Spark data frame
#' @param hive.table Hive table to insert into
#' @param partition Partition name and value
#' @param spark_tbl.name A name for spark data frame
#'
#' @examples
#' \dontrun{
#' insertIntoHiveTable(
#'   spark_df = my_copied_to_tbl,
#'   hive.table = lab.my_table,
#'   partition = "id = '123'"
#' )
#' }
#'
insertIntoHiveTable <-
  function(sc, spark_df, hive.table, partition, spark_tbl.name) {

    insert_into_sql <- paste(
      "insert into table", hive.table,
      "partition (", partition, ")",
      "select * from", spark_tbl.name
    )

    sparklyr::hive_context(sc) %>%
      sparklyr::invoke("sql", insert_into_sql)
  }


#' wrapper for createHiveTable and insertIntoHiveTable functions
#'
#' @param sc A spark connection
#' @param df A data frame
#' @param spark_tbl.name A name for spark data frame (copied to)
#' @param hive.table The hive table name to create or to insert into
#' @param partitionBy Partition column
#' @param partition The partition name with value
#' @param report_progress A logging interface
#'
#' @examples
#' \dontrun{
#' uploadToBDP(
#'   sc = sc,
#'   df = iris,
#'   spark_tbl.name = "iris_tbl",
#'   hive.table = "lab.my_hive_iris_table",
#'   partitionBy = "session_id",
#'   partition = "session_id = 123",
#'   report_progress = report_progress
#' )
#' }

uploadToBDP <- function(sc,
                        df = stop(),
                        spark_tbl.name = "assort_table_to_upload",
                        hive.table = stop(),
                        partitionBy = c("session_id", "prod_division"),
                        partition,
                        report_progress) {

  report_progress <- report_progress$subarea(hive.table)

  report_progress$info("uploadToBDP...")

  report_progress$info("sparklyr::sdf_copy_to...")
  spark_tbl <-
    sparklyr::sdf_copy_to(sc, df, spark_tbl.name, overwrite = TRUE)
  report_progress$info("sparklyr::sdf_copy_to ok")

  report_progress$info("createHiveTable...")
  createHiveTable(sc,
                  name = hive.table,
                  columns = colnames(spark_tbl),
                  partitionBy = partitionBy)
  report_progress$info("createHiveTable ok")

  report_progress$info("insertIntoHiveTable...")
  insertIntoHiveTable(sc,
                      spark_df = spark_tbl,
                      hive.table = hive.table,
                      partition = partition,
                      spark_tbl.name = spark_tbl.name)
  report_progress$info("insertIntoHiveTable ok")

  report_progress$info("uploadToBDP ok")
}
