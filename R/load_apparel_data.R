#' load_apparel_data
#'
#' Function that load the input data for apparel
#'
#' @param technical_settings A wraper function of the technical set up
#' @param analytical_settings A wraper function of the analytical settings
#' @param spark_context A wraper function for the spark connection
#' @param progress_report is used for logging
#'
#' @return The data required to run the analysis
#'
#' @author Vadim Zayakin <vadims.zajakins@@accenture.com>
#' @author Fortuny Sicart, Alan <Alan.FortunySicart@@adidas-group.com>
#'
load_apparel_data <-
  function(technical_settings,
           analytical_settings,
           spark_context,
           progress_report) {

    progress_report$info(paste0('Using sales data from [', analytical_settings$table_sales_attributes, ']'))

    #Read the data that contain the attributes and sales
    data <-
      utils::read.csv2(analytical_settings$table_sales_attributes,
                       sep = ",",
                       header = T)

   # validate_csv(data, progress_report)

    # Adjust column names
    names(data)[names(data) == analytical_settings$article_id_name_sales] <-
      "Article_Number"
    names(data)[names(data) == analytical_settings$price_name_ap] <-
      "ARP"
    names(data)[names(data) == analytical_settings$sales_name_ap] <-
      "norm_sales"
    names(data)[names(data) == analytical_settings$fabric] <-
      "fabric"

    article_apparel <-
      sqldf::sqldf("select distinct Article_Number from data")

    #Push article list in scope into Spark
    articles_in_scope <-
      dplyr::copy_to(spark_context, article_apparel)

    #Get some data for the views from v_article_rfd
    v_article <-
      dplyr::tbl(spark_context,
                 dplyr::sql(analytical_settings$sql_txt_v_article_rfd))

    #Keep the rest of the attributes only for the articles in scope
    v_article_intermediate <-
      v_article %>% dplyr::inner_join(articles_in_scope, by = c("article_no" = "Article_Number"))
    v_article_full <- v_article_intermediate %>% dplyr::collect()

    # Return in a list form the data for analysis and the data with the view information
    list(data = data,
         v_article_full = v_article_full)
  }
