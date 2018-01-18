#' load_footwear_data
#'
#' Function that load the input data for footwear
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
load_footwear_data <-
  function(technical_settings,
           analytical_settings,
           spark_context,
           progress_report) {

    progress_report$info(paste0('Using sales data from [', analytical_settings$table_sales, ']'))

    #read the data that contain the attributes
    #Create the consolidated PDNA Color and Material table
    attribute_data_list <-
      footwear_pdna_data_preparation(analytical_settings,
                                     technical_settings,
                                     spark_context)

    #PDNA Color and Material table
    attribute_data <- attribute_data_list[[1]]
    #Rest of the attributes like Gender, Age, Sports Category, Product Type Desc...
    v_article_full <- attribute_data_list[[2]]

    progress_report$info("Footwear data pulled")

    # get the net sales, key franchise data
    sales_data <-
      utils::read.csv(
        paste0(analytical_settings$table_sales),
        header = TRUE,
        sep = ",",
        dec = "."
      )

    validate_csv(sales_data, progress_report)

    #Merge Attribute data with Sales data
    data <- merge(
      x = attribute_data,
      y = sales_data[, c(
        "Article","Silhouette",
        analytical_settings$sales_name_fw,
        analytical_settings$price_name_fw,
        'Grouping'
      )],
      by = "Article",
      all.x = TRUE
    )

    progress_report$info("Footwear data merged")

    # Adjust column names
    names(data)[names(data) == analytical_settings$price_name_fw] <-
      "ARP"
    names(data)[names(data) == analytical_settings$sales_name_fw] <-
      "norm_sales"
    names(data)[names(data) == analytical_settings$article_id_name_sales] <-
      "Article_Number"
    names(data)[names(data) == analytical_settings$fabric] <-
      "fabric"

    list(data = data,
         v_article_full = v_article_full)
  }
