#' Analytical setting apparel
#'
#' The user is able to specify several settings regarding the analysis itself.
#'
#' @param saveResults A boolean varible set by the user when he wants to save the results or not
#' @param table_sales_attributes A table provided by the BUs with the article id, the sales or forecasts, the margin, the BV, the Grouping and some attributes
#' @param sql_txt_v_article_rfd describes the columns selected from this table
#' @param output_path it specifies the path where the results are going to be stored
#' @param sales_name the name of the column sales in the dataset
#' @param price_name the name of the column price in the dataset
#' @param article_id_name_sales the name of the column article_id in the dataset of BUs
#' @param fabric the name of the column fabric in the dataset
#' @param split_by_price A boolean value indicating if spliting by price should be performed - receives "YES" and "NO" values
#' @param price_cluster_criteria A string, if informed as "Manual", will use the manual price segments, otherwise, automatic segments will be applied
#' @param price_cluster_number  A number defining the number of preferable clusters to group the price when the criteria is set to "Automatic"
#' @param price_threshold A numeric vector indicating the thresholds used to split by price
#' @param split_by_fabric A boolean value indicating if spliting by fabric should be performed - receives "YES" and "NO" values
#' @param group_fabric a boolean value indicating if you want to group fabric at higher level
#' @param fabric_threshold A numeric vector indicating the thresholds used to group the fabric
#' @param atributte_levels A numeric value indicating the maximunm level per attribute. If a variable exceeds this number then the rest of its levels are grouped to 'other'.
#' @param atributte_theshold A numeric value indicating what is the minimum percentage of importance that we use to select the most important primary attributes on the random forest
#' @param maximum_pvalue A number indicating the maximum pvalue accepted on the variables (default value is 0.5)
#' @param max_weight  A number indicating the maximum of the weight of the primary attributes (calculated as the median impact of secondary levels)
#' @param combi A number indicating the number of variable combinations to be tested in the attribute regression (minimum value is 5)
#' @param retain_threshold A numeric value indicating the percentage of sales that need to be retained from the deletion of articles
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#' @return The analytical settings
#' @examples
#'
#' get_analytical_settings()
get_analytical_settings = function() {
  analytical_settings = list(


    #basic name links
    Article_id="Article_Number",
    Volume="norm_sales",
    Price="ARP",
    Grouping="Grouping",
    fabric = "fabric",

    #sample size threshold
    sample_size_thresh=30,


    # Save Results
    saveResults = "TRUE",

    # footwear-specific settings:
    sql_txt_apdm_bom = "select article,season,production_factory,part_number,part_name,material_class,adidas_color_code,color_name from lab.apdm_bom",
    sql_txt_apdm_template = "select part_number,part_name,part_location,part_area from lab.apdm_template ",
    sql_txt_rfd_color_stg1 = "select color_cd,color_grp from lab_landing.rfd_color_stg1",

    table_articles_in_scope = "data/FTW_SS18_NAM/articles_ftwr.Rda",
    table_sales = "data/FTW_SS1819_ext_NAM/NAM_ext_SS18_SS19_FTW.csv",
    sales_name_fw = "Volume",
    price_name_fw = "RRP",
    output_path_fw = "output",


    # apparel-specific settings:
    table_sales_attributes = "data/APP_SS18_SS19_ext_NAM/NAM_ext_SS18_SS19_APP.csv",
    sales_name_ap = "norm_sales",
    price_name_ap = "ARP",
    output_path_ap = "output",

    # common settings:
    sql_txt_v_article_rfd = "select article_no,
                                    brand_cd,
                                    brand_desc,
                                    sports_cat_desc,
                                    prod_div_desc,
                                    mktg_div_cd,
                                    mktg_div_desc,
                                    key_cat_desc,
                                    bus_unit_cd,
                                    bus_unit_desc,
                                    bus_seg_desc,
                                    prod_grp_desc,
                                    prod_type_desc,
                                    rmh_mktg_div_desc,
                                    rmh_gender_desc,
                                    age_group_desc
                                    from lab_out.v_article_rfd",
    article_id_name_sales = "Article_Number",
    Grouping="Grouping",
    fabric = "fabric",

    # Settings for Price and Fabric splits
    split_by_price = "YES",
    price_cluster_criteria = "Automatic",
    price_cluster_number = 2,
    price_threshold = NA,
    split_by_fabric = "YES",
    group_fabric = "YES",
    fabric_threshold = c(0,150,250,500,1000),

    # Settings for The Variable Reduction Selection
    atributte_levels = 8,
    atributte_theshold = c(5,4,3,2,1),

    # Settings for The Attribute Regression
    maximum_pvalue = 0.5,
    max_weight = 0.6,
    combi = c(30,100,1000,5000,20000),

    # Settings for The Transferability Analysis
    retain_threshold = 0.99,

    .last = NA
  )

  class(analytical_settings) = "analytical_settings"
  return(analytical_settings)
}
