#'
#' process_assortment
#'
#' Assortment Analysis - main entry point
#'
#' Defines callback functions:
#'  - for upload tables to hive
#'  - for store detailed results in files
#'  - for capturing application log
#' Generates unique session id
#' Calls the analysis twice: for apparel and footwear.
#'
#' @param  technical_settings   Environment (development/testing/production) parameters
#' @param  analytical_settings  Analysis customization anf fine tuning parameters
#' @param  spark_context        The Spark connection
#'
#' @return NA
#'
#' Side-effects:
#'   - added records to hive tables
#'   - created set of file in 'output' directory
#'
#' @author Vadim Zayakin <vadims.zajakins@accenture.com>
#' @author Fortuny Sicart, Alan <Alan.FortunySicart@adidas-group.com>
#'
process_assortment <-
  function(technical_settings,
           analytical_settings,
           spark_context) {

    # Process assortment calls a function called upload proc, which push the output data to the bdp
    # We will need to specify the data frame, data name, session id and prod_division
    upload_proc <-
      function(df,
               table,
               session_id,
               prod_division,
               report_progress) {
        hive.table <- paste0(technical_settings$database$output, ".", table)
        report_progress$info(paste("insert into", hive.table, "..."))
        report_progress$info(utils::head(df))
        uploadToBDP(
          sc = spark_context,
          df = df,
          #df refers to the data frame created,
          hive.table = hive.table,
          #table is the table name to be added on the bdp
          partition = paste0(
            "session_id = ",
            session_id,
            ", ",
            "prod_division = '",
            prod_division,
            "'"
          ), #partition indicated the analysis identifier for the specific product division
          report_progress = report_progress
        )
        report_progress$info(paste("insert into", hive.table, "ok"))
      }

    # Save to BDP factory is a wraper of upload proc for each output data to be send into the bdp
    save_to_bdp_factory <-
      function(session_id,
               prod_division,
               report_progress) {
        function(data_prepared_to_upload,
                 attribute_importance_table,
                 attribute_importance_table_reg,
                 sec_attribute_importance_table_reg,
                 top5_transfer_demand) {
          upload_proc(
            data_prepared_to_upload,
            "assort_input_data_range_optimization",
            session_id,
            prod_division,
            report_progress
          )
          upload_proc(
            attribute_importance_table,
            "assort_attribute_selection",
            session_id,
            prod_division,
            report_progress
          )
          upload_proc(
            attribute_importance_table_reg,
            "assort_attribute_importance",
            session_id,
            prod_division,
            report_progress
          )
          upload_proc(
            sec_attribute_importance_table_reg,
            "assort_secondary_attribute_importance",
            session_id,
            prod_division,
            report_progress
          )
          upload_proc(
            top5_transfer_demand,
            "assort_top5",
            session_id,
            prod_division,
            report_progress
          )
        }
      }

    report_progress_factory('technical settings')$info(utils::capture.output(str(technical_settings)))
    report_progress_factory('analytical settings')$info(utils::capture.output(str(analytical_settings)))

    # Create session id
    session_id <- gsub(
      "[^[:digit:]]",
      "",
      format(
        Sys.time(),
        tz = "CET"
      )
    )

    area <- 'Apparel'

    # Create path for local saving
    save_path <- save_path_factory(session_id, area)

    # Report code progress
    report_progress <- report_progress_factory(area)

    # Create path for the bdp
    save_to_bdp <-
      save_to_bdp_factory(session_id, area, report_progress)

    #Load APP data
    loaded_data <- load_apparel_data(technical_settings,
                                     analytical_settings,
                                     spark_context,
                                     report_progress)
    #Run analysis
    assortment_analysis(
      technical_settings,
      analytical_settings,
      loaded_data,
      save_path,
      save_to_bdp,
      report_progress
    )

    area <- 'Footwear'

    # Create path for local saving
    save_path <- save_path_factory(session_id, area)

    # Report code progress
    report_progress <- report_progress_factory(area)

    # Create path for the bdp
    save_to_bdp <-
      save_to_bdp_factory(session_id, area, report_progress)

    #Load FTW data
    loaded_data <- load_footwear_data(technical_settings,
                                      analytical_settings,
                                      spark_context,
                                      report_progress)
    #Run analysis
    assortment_analysis(
      technical_settings,
      analytical_settings,
      loaded_data,
      save_path,
      save_to_bdp,
      report_progress
    )

    return(NA)
  }
