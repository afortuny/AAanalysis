#' technical_settings.R
#'
#' Technical settings for production environment.
#'
#' @param spark.dynamicAllocation.enabled A boolean value specifying whether to use dynamic resource allocation, which scales the number of executors registered with this application up and down based on the workload. The default value is False.
#' @param spark.dynamicAllocation.maxExecutors A number specifying the Upper bound for the number of executors if dynamic allocation is enabled. The default value is infinity.
#' @param spark.executor.cores The number of cores to use on each executor
#' @param spark.executor.memory A number indicating the amount of memory to use per executor process (eg. 1g)
#' @param 'sparklyr.shell.driver-memory'
#' @param spark.driver.maxResultSize A number indicating the limit of total size of serialized results of all partitions for each Spark action (eg. 1g)
#' @param spark.yarn.queue the name of the YARN queue to which the application is submitted
#' @param temp
#' @param config
#' @param input the input dataset
#' @param output the output dataset
#' @param temp_dir a directory for temporary files the application may use
#' @param master 'yarn-client' | 'local'
#' @param version expected version of Spark
#' @param config the custom configuration for the generated Spark connection
#' @param spark_home A path with the location where Spark is stored
#' @param app_name A name of the application visible in Spark logs
#'
#' @return The technical settings
#'
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#'
get_technical_settings <- function() {

  # Spark configuration ---------------------------------------
  spark_settings <- c(
    sparklyr::spark_config(),
    list(
      spark.dynamicAllocation.enabled      = TRUE,
      spark.dynamicAllocation.maxExecutors = 5,
      spark.executor.cores                 = 1,
      spark.executor.memory                = "1G",
      'sparklyr.shell.driver-memory'       = "2G",
      spark.driver.maxResultSize           = 0,
#     spark.kryoserializer.buffer.max      = "64M",
#     spark.ui.port                        = '4040',
#     sparklyr.gateway.port                = '8880',
      spark.yarn.queue                     = "ds"
    )
  )

  # Databases -------------------------------------------------
  database <-
    list(
      temp        = 'lab',     # temporary tables
      config      = 'lab',     # configuration database
      input       = 'lab',     # input database
      output      = 'lab'      # output database
    )

  # Tables ----------------------------------------------------
  table =
    list(
      request     = paste0(database$config, '.', 'ds_assortment_request'),
      status      = paste0(database$config, '.', 'ds_assortment_status'),
      history     = paste0(database$config, '.', 'ds_assortment_sthist'),
      scope       = paste0(database$config, '.', 'ds_assortment_scope'),
      sales       = paste0(database$config, '.', 'ds_assortment_sales'),
      attributes  = paste0(database$config, '.', 'ds_assortment_attribute'),
      views       = paste0(database$config, '.', 'ds_assortment_views'),

      attribute_importance = paste0(database$output, '.', 'ds_assortment_attribute_importance'),
      transferability      = paste0(database$output, '.', 'ds_assortment_transferability')
    )

  # Fields ----------------------------------------------------
  field =
    list(
      request          = 'request_id',
      article          = 'article_id',
      season           = 'season',
      market           = 'market_id',
      prod_division    = 'prod_division',
      description      = 'description',
      comment          = 'comment',
      status           = 'status',
      log              = 'app_log',
      timestamp        = 'timestamp',
      grouping         = 'grouping',
      key              = 'attr_key',
      value            = 'attr_value',
      sales            = 'volume',
      price            = 'price',

      primary_attribute_name           = 'primary_attribute_name',
      primary_attribute_value          = 'primary_attribute_value',
      secondary_attribute_name         = 'secondary_attribute_name',
      secondary_attribute_name_grouped = 'secondary_attribute_name_grouped',
      secondary_attribute_value        = 'secondary_attribute_value',
      rank                             = 'rank',

      article_to_keep                  = 'article_id_to_keep',
      article_to_delete                = 'article_id_to_delete',
      transferability_percentage       = 'transferability_percentage',
      ranking_to_delete                = 'ranking_to_delete',
      absolute_sales_transferred       = 'absolute_sales_transferred',
      new_total_sales_receiving        = 'new_total_sales_receiving',
      sales_to_delete                  = 'sales_to_delete',
      total_sales_transferability      = 'total_sales_transferability',
      sales_lost_relative              = 'sales_lost_relative',
      sales_lost_absolute              = 'sales_lost_absolute'
    )

  # Technical settings ----------------------------------------
  technical_settings <- list(

    spark_settings = spark_settings,
    temp_dir = '.R-temp',

    database = database,
    table = table,
    field = field,

    spark_connect = function(app_name = 'assortment')
      sparklyr::spark_connect(
        master     = 'yarn-client',
        version    = '1.6.0',
        config     = spark_settings,
        spark_home = '/opt/cloudera/parcels/CDH/lib/spark/',
        app_name   = app_name
      ),

    spark_disconnect = function()
      sparklyr::spark_disconnect_all()
  )

  class(technical_settings) <- "technical_settings"
  return(technical_settings)
}
