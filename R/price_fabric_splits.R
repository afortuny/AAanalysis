#' Price and fabric Splitting
#'
#' Due to the fact that all variables in the model will be treated as categorical, we group price and fabric weight.
#' Exif the fabric variable exists, it prints the fabric distribution and if split_by_fabric is set to "YES", it creates a new table including the fabric_group variable used for the splitting.
#' If fabric_group is set to 'YES' then it groups the initial fabric variable to higher level based on the value set by the user in the fabric_threshold.
#' it prints the price distribution and creates price clusters based on an "Automatic" or "Manual" procedure.
#' If "Automatic" it uses K-Means to group the price. If the set is used to "Manual" then the user can set his own price thresholds
#'
#' requires grDevices and stats package
#'
#' @param split_by_fabric A boolean vector indicating if spliting by factor should be performed - receives "YES" and "NO" values
#' @param fabric_threshold A numeric vector indicating the thresholds used to split by fabric. By default 0-149,150-249,250-499,500-1000 is provided
#' @param group_fabric a text string indicating if group of fabric is required ("YES") or not ("NOT")
#' @param data_group input dataset containing all the attributes and the price and fabric weight (for apparel)
#' @param split_by_price A boolean vector indicating if spliting by price should be performed - receives "YES" and "NO" values
#' @param price_cluster_criteria A string, if informed as "Manual", will use the manual price segments, otherwise, automatic segments will be applied
#' @param price_cluster_number A number defining the number of preferable clusters when the criteria is set to "Automatic"
#' @param price_threshold A numeric vector indicating the thresholds used to split by price. This is used when user have set price_cluster_criteria equal to Manual
#' @param report_progress A logging interface
#' @param fabric A text string indicating the column name of fabric
#' @param Price A text string indicating the column name of Price
#' @param Article_id A text string indicating the column name of Article_id
#'
#' @author Alan Fortuny <Alan.FortunySicart@@adidas-group.com>
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#' @author Ilektra Kocheila <ilektra.kocheila@@accenture.com>
#' @return a dataset that is similar to the input dataset but includes (a) the group fabric variable depending on the parameter selection, (b) the price grouping
#' @examples
#' \dontrun{
#' #Define primary attributes based on global attribute creation function
#'price_fabric_splits(split_by_fabric="YES",
#'fabric_threshold=200,
#'data_group_v0,
#'split_by_price = "YES",
#'price_cluster_criteria= "Manual",
#'price_cluster_number=NA,
#'price_threshold=c(60))
#'}
price_fabric_splits <- function(
                                fabric_threshold=c(0,150,250,500,1000),
                                group_fabric = "YES",
                                data_group,
                                price_cluster_criteria = "Automatic",
                                price_cluster_number = 2,
                                price_threshold=NA,
                                report_progress,
                                fabric="fabric",
                                Price="ARP",
                                Article_id="Article_Number") {


  #FABRIC TREATMENT
  #Examine if fabric exists in dataset with the expected name, if exist with other naming will be treated as a attributed
  if (fabric %in% names(data_group)){

    report_progress$info("fabric found")

  #when splitting by fabric is set to "YES", examine if the fabric levels are sufficient to create a fabric group variable
    #If fabric group is set to "YES" then we aggregate the fabric weight to higher level hierarchy
    if (group_fabric == "YES" &
        length(levels(data_group$fabric)) > 2)
    {
      #apply fabrics transformation and create the fabric group variable


      fabric_limits <-
        fabrics_transf(data_group=data_group)

      #if there is a NA report and eliminate those cases
      if(any(is.na(fabric_limits[,"lim"]))){

        unvalid_fabric_weights<-as.character(fabric_limits[which(is.na(fabric_limits[,"lim"])),fabric])
        report_progress$error(paste0("unvalid fabric found"," ",paste0(unvalid_fabric_weights,collapse=", " )))

        report_progress$error(paste0("Articles " ,paste0(data_group[which(data_group[,fabric]==unvalid_fabric_weights),Article_id],collapse=" ")," drop due to wrong fabric"))

        data_group<-data_group[-which(data_group[,fabric]==unvalid_fabric_weights),]


      }



      #get the minimum and maximum and the segments
      min_fabric <- min(fabric_limits$lim,na.rm=TRUE) - 1
      max_fabric <- max(fabric_limits$lim,na.rm=TRUE)
      man_fabric_seg = c(min_fabric, fabric_threshold, max_fabric)


      #create the cuts
      fabric_limits[, "lim"] <-cut(fabric_limits[, "lim"], breaks = man_fabric_seg)

      # add the column
      data_group <-
        sqldf::sqldf(
          "select a.*, b.lim as fabric_group
          from data_group as a
          left join
          fabric_limits as b
          on a.fabric=b.fabric"
        )
    }



  } else {

    report_progress$info("fabric NOT found")
    }



  #PRICE TREATMENT
  #If any of the parameters are provided in a wrong manner the automatic approach will be followed
  #Prices needs to be cathegorized whether is used in the split or as an attribute
  #If price cluster criteria is manual, price threshold needs to be informed, otherwise automatic will be used
  min_price<-min(data_group[,Price])
  max_price<-max(data_group[,Price])

  if(price_cluster_criteria=="Manual" & !is.na(price_threshold) & min(price_threshold)<=min_price & max(price_threshold)>=max_price ){

    data_group[, 'price_cat'] <-
      cut(data_group[, Price], breaks = price_threshold)

    report_progress$info("Manual price grouping used")

  }else{

    #extract the min,q1,q2,mean,q3,max
    price_range_splits <-grDevices::boxplot.stats(data_group[, Price])
    # check whether a split is meaningfull based on price range
    # If IQR covers more than the 70% of the range of price (max - min) then do not apply any split

  IQRperc<-(price_range_splits$stats[4]-price_range_splits$stats[2])/(price_range_splits$stats[5]-price_range_splits$stats[1])
  if(is.nan(IQRperc)){

    IQRperc<-1
  }

  if (IQRperc > 0.7 |price_range_splits$stats[5]-price_range_splits$stats[1]==0) {

    data_group[,"price_cat"] <-paste0("(",min_price-1,",",max_price,"]")
    report_progress$info("Only one group for price split will be used due to lack of variation")

  } else {

      #if MISSING then automatic, the price breaks will be based on kmeans with
      #else the price_cluster_number is used as set by the user
      if (is.na(price_cluster_number) & price_cluster_number>1) {
        CluR <- stats::kmeans(data_group[, Price], centers = price_cluster_number)

        report_progress$info("Two automatic clusters used due to wrong auntomatic price number indicated")

      } else {
        CluR <-
          stats::kmeans(data_group[, Price], centers = price_cluster_number)

        report_progress$info("Automatic price groups used")
      }

    #assign price clusters

    data_group$price_cluster <-
      CluR$cluster

    #get the max price per cluster

    ARP<-Price
    auto_price_seg <-
      aggregate(ARP ~ price_cluster, data_group, function(x)
        max(x))

    #round the price
    auto_price_seg <-
      sort(as.vector(round(auto_price_seg[,Price], 0)))


    #Clustering is based on the level of clusters he user has indicated
    #we add one to the last group max to avoid missing the true max because of using round
    auto_price_seg <- c(min_price - 1, sort(auto_price_seg))
    data_group[, 'price_cat'] <-
      cut(data_group[, Price], breaks = auto_price_seg)
    data_group <-
      within(data_group, rm('price_cluster'))


}
}


  #at the end, Price will be removed since only categorized price in segments will be used

  data_group <-data_group[,-which(colnames(data_group) %in% Price)]

  return(data_group)
}
