#' Clusters combinations
#'
#' It creates the combination of price and fabric levels within which we allow demand transferability. The output is the number of clusters and the repsective articles that fall within those clusters that
#' are going to be examined in the transferability function.
#'
#' @param split_by_price A string vector indicating if spliting by price should be performed - receives "YES" and "NO" values
#' @param split_by_fabric A string vector indicating if spliting by fabric should be performed - receives "YES" and "NO" values
#' @param range_data_attrib_all The input dataset
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#' @author Ilektra Kocheila <ilektra.kocheila@@accenture.com>
#' @return a dataset that is similar to the input dataset but includes the clusters variable
#' @return a string vector including the distinct clusters on which the split will be performed
#' @examples
#' \dontrun{
#' #Run the function
#' clusters_combi(split_by_price="NO",
#' split_by_fabric="NO",
#' range_data_attrib_all)
#'}


clusters_combi <- function(split_by_price = "NO",
                           split_by_fabric = "NO",
                           range_data_attrib_all)

{
  if ("fabric" %in% names(range_data_attrib_all))
  {
    #add the fabric group variable to the range_data_attrib_all dataset, if split_by_fabric is "NO" a variable fabric group is created that has the value of 1
    if (split_by_fabric == "YES")
    {
      if ("fabric_group" %in% names(range_data_attrib_all)) {
        range_data_attrib_all <- range_data_attrib_all
      } else
      {
        range_data_attrib_all <- sqldf::sqldf(
          "select a.*, b.fabric as fabric_group
          from range_data_attrib_all as a
          left join
          data_mart_assort_ARP_clean as b
          on a.Article_Number=b.Article_Number"
        )
      }

      } else {
        range_data_attrib_all$fabric_group <- 1
      }
    }


  #if split_by_price is "NO" the variable price_cat takes the value of 1
  if (split_by_price == "NO")
  {
    range_data_attrib_all$price_cat <- 1
  }

  #create the clusters variable
  if ("fabric" %in% names(range_data_attrib_all))
  {
    range_data_attrib_all$clusters <-
      paste(range_data_attrib_all$price_cat,
            range_data_attrib_all$fabric_group)
  } else if ("fabric_group" %in% names(range_data_attrib_all))
  {
    range_data_attrib_all$clusters <-
      paste(range_data_attrib_all$price_cat,
            range_data_attrib_all$fabric_group)
  } else {
    range_data_attrib_all$clusters <- range_data_attrib_all$price_cat
  }

  clusters <- unique(range_data_attrib_all$clusters)

  return(list(range_data_attrib_all, clusters))

  }
