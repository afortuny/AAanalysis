#' Transferable Demand
#'
#' Performs an iterative process of deleting articles with low incremental sales and redistributing their sales to those that have same attributes. The allocation is done based on the significance of the attributes and normalized sales. The algorithm estimates the amount of sales that are being lost and the amount of sales that are being transfered to the rest of the portfolio.
#'
#' requires scale, reshape and ggplot2 package
#' @param raw_range_data_att A data frame with the articles in scope, with normalized and relevant attributes with all levels
#' @param prim_attr_weights A numeric vector indicating the weight of each primary attribute based on the regression analysis
#' @param analysis_filters  A string vector indicating the criteria that define the analysis group
#' @param retain_threshold A numeric value indicating the percentage of sales that need to be retained from the deletion of articles
#' @param portfolio_threshold A numeric value to indicate which is the maximum article drop %
#' @param primary_attributes A string vector indicasting the primary attributes names
#' @param grouping A string vector indicating grouping
#' @param  Article_id A text string indicating the column name of Article_id
#' @param  Volume A text string indicating the column name of norm_sales
#' @param report_progress A logging interface
#'
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#' @author Alan Fortuny <Alan.FortunySicart@@adidas-group.com>
#' @return A list with the top5 report
#' @examples
#' \dontrun{
#'   transferable_demand(raw_range_data_att=raw_range_data_att,
#'   prim_attr_weights=c(0.2394654,0.2253099,0.2247120,0.3105127),
#'   analysis_filters=1,
#'   retain_threshold=0.99,
#'   portfolio_threshold=0.5,
#'   data,
#'   primary_attributes=primary_attributes,
#'   wd=~/Projects/Assortment Apparel SS18
#'   saveResults = TRUE)
#'}
#'
transferable_demand <- function(raw_range_data_att,
                                prim_attr_weights,
                                analysis_filters,
                                retain_threshold = 0.99,
                                portfolio_threshold = 0.5,
                                primary_attributes,
                                report_progress,
                                grouping,
                                Article_id = "Article_Number",
                                Volume = "norm_sales") {



raw_range_data_att[,Volume]<-as.numeric(as.character( raw_range_data_att[,Volume]))

raw_range_data_att[,Article_id]<-as.character(raw_range_data_att[,Article_id])

#create top5
top5<-matrix(ncol=6)

colnames(top5)<-c("article_to_delete","article_to_keep","demand_transfer","demand_transfer_perc","sales_to_delete","grouping")
top5<-top5[-1,]
# Calculate for the first the incremental sales
data <-
  computation_inc_sales(
    raw_range_data_att = raw_range_data_att,
    prim_attr_weights = prim_attr_weights,
    analysis_filters = analysis_filters,
    primary_attributes=primary_attributes
  )


# NOTE! the threshold is on incremental sales not total sales lost

Cummulative_Incrementality <- as.numeric(sum(data$inc_sales))

loss_perc <- min(data$inc_sales) / Cummulative_Incrementality


##################
# Initial Values for Portfolio Threshold & Initial Retain Percentage.
# Portfolio Threshold is the percentage of articles that above which no more articles should be deleted.

portfolio = 1 / nrow(raw_range_data_att)
art=1

# Empty intermediate tables
demand_transferred_att <- matrix(ncol = 4)
colnames(demand_transferred_att) <-
  c("article_to_delete",
    "article_to_keep",
    "attribute",
    "demand_transfer")



temp_range<-raw_range_data_att


if (art < portfolio_threshold*nrow(raw_range_data_att) &
    loss_perc < (1 - retain_threshold) &
    portfolio < portfolio_threshold) {





while (art < portfolio_threshold*nrow(raw_range_data_att) &
       loss_perc < (1 - retain_threshold) &
       portfolio < portfolio_threshold) {

  #identify the article with less incremental sales

  ind_del <- which.min(data[, "inc_sales"])
  temp_range <-temp_range[-which(temp_range[, Article_id] %in% data[ind_del, Article_id]), ]

  for (a in primary_attributes) {
    ind_transfer <- which(data[, a] == data[ind_del, a])
    ind_transfer <- subset(ind_transfer, ind_transfer != ind_del)
    if (length(ind_transfer) == 0) {
      next
    } else {
      weights_transferred <-
        data[ind_transfer, paste0("Vol", a)] / sum(data[ind_transfer, paste0("Vol", a)])
      sales_transferred <-
        weights_transferred * data[ind_del, paste0("vol_transferred_", a)]
      sales_transferred <-
        as.data.frame(cbind(data[ind_del, Article_id], data[ind_transfer, Article_id], a, sales_transferred))
      colnames(sales_transferred) <-
        c("article_to_delete",
          "article_to_keep",
          "attribute",
          "demand_transfer")
      demand_transferred_att <-
        rbind(demand_transferred_att, sales_transferred)
    }
  }

  #if none of the demand is transfer, everthing is lost
  if (all(is.na(demand_transferred_att))) {
    demand_transferred_att <- as.data.frame(demand_transferred_att)
    demand_transferred_att[, "article_to_delete"] <-
      data[ind_del, c(Article_id)]
    demand_transferred_att[, "article_to_keep"] <- "NONE"
    demand_transferred_att <- demand_transferred_att[, -c(3)]
    demand_transferred_att[, "demand_transfer"] <- 0
    demand_transferred_att[, "demand_transfer_perc"] <- 0
    demand_transferred_att[, "sales_to_delete"] <-
      data[ind_del, Volume]



  } else {

    #aggregate transfer demand per article
    demand_transferred_att <-
      demand_transferred_att[complete.cases(demand_transferred_att),]
    demand_transferred_att[, "demand_transfer"] <-
      as.numeric(demand_transferred_att[, "demand_transfer"])
    demand_transferred_att <-
      aggregate(
        demand_transfer ~ article_to_delete + article_to_keep,
        demand_transferred_att,
        FUN = sum
      )
    demand_transferred_att[, "demand_transfer_perc"] <-
      (demand_transferred_att[, "demand_transfer"] / data[ind_del, Volume]) *
      100
    demand_transferred_att[, "sales_to_delete"] <-
      data[ind_del, Volume]

    #update sales figures on remaining articles

    temp_range <-
      merge(
        temp_range,
        demand_transferred_att[, c("demand_transfer", "article_to_keep")],
        by.x = c(Article_id),
        by.y = c("article_to_keep"),
        all.x = TRUE
      )
    temp_range[, "demand_transfer"] <-
      replace(temp_range[, "demand_transfer"], is.na(temp_range[, "demand_transfer"]), 0)
    temp_range[, Volume] <-
      temp_range[, "demand_transfer"] + as.numeric(as.character(temp_range[, Volume]))
    temp_range <-
      temp_range[, -which(colnames(temp_range) %in% c("demand_transfer"))]


  }

  #create top5 report
  top5_temp <-cbind(demand_transferred_att[order(-demand_transferred_att[, "demand_transfer_perc"]), ], grouping)
  top5<-rbind(top5,top5_temp)


  # Empty intermediate tables
  demand_transferred_att <- matrix(ncol = 4)
  colnames(demand_transferred_att) <-
    c("article_to_delete",
      "article_to_keep",
      "attribute",
      "demand_transfer")



  # Calculate for the first the incremental sales
  data <-
    computation_inc_sales(
      raw_range_data_att =temp_range,
      prim_attr_weights = prim_attr_weights,
      analysis_filters = analysis_filters,
      primary_attributes=primary_attributes
    )

  #update sales loss
  loss_perc <-
    loss_perc + min(data[, "inc_sales"]) / Cummulative_Incrementality
  #update portfolio deleted
  portfolio <- portfolio + 1 / nrow(raw_range_data_att)

  #update art
  art=art+1



}

if (all(!is.na(top5))) {
  report_progress$info("Article deletion executed")



  # eliminate those transfer demand cases where the article was deleted afer
  deleted_art <-
    top5[!duplicated(top5[, "article_to_delete"]), "article_to_delete"]

  top5 <- subset(top5, !(top5[, "article_to_keep"] %in% deleted_art))

  # recalculate totals
  total_demand_transferred <-
    aggregate(demand_transfer_perc ~ article_to_delete, top5, FUN = sum)

  colnames(total_demand_transferred)[2] <-
    "total_sales_transfer_relative"
  total_demand_transferred[, "total_sales_lost_relative"] <-
    100 - total_demand_transferred[, "total_sales_transfer_relative"]

  # add updated total sales lost to top5

  top5 <-
    merge(top5, total_demand_transferred, by = c("article_to_delete"))
  top5[, "sales_lost_absolut"] <-
    (top5[, "total_sales_lost_relative"] / 100) * top5[, "sales_to_delete"]


  # add ranking
  ranking <- c()
  for (ad in deleted_art) {
    ranking_t <-
      as.data.frame((c(1:length(
        subset(top5, top5[, "article_to_delete"] == ad)[, 1]
      ))))
    ranking <- rbind(ranking, ranking_t)
  }

  top5[, "ranking"] <- ranking



  #aggregate the total sales transfer by article to keep and merge it with the top 5
  total_demand_transferred <-
    aggregate(demand_transfer ~ article_to_keep, top5, FUN = sum)

  raw_range_data_att[,Article_id]<-as.character(raw_range_data_att[,Article_id])

  total_demand_transferred <-
    merge(
      total_demand_transferred,
      raw_range_data_att[, c(Article_id, Volume)],
      by.x = c("article_to_keep"),
      by.y = c(Article_id)
    )
  total_demand_transferred[, "new_total_sales_keep"] <-
    total_demand_transferred[, "demand_transfer"] + total_demand_transferred[, Volume]
  top5 <-
    merge(
      top5,
      total_demand_transferred[, c("article_to_keep", "new_total_sales_keep")],
      by = c("article_to_keep"),
      all.x = TRUE
    )
  #filter only for those cases in the ranking between 1 and 5
  transferability_all <- top5
  top5 <- subset(top5, top5[, "ranking"] < 6)
  return(list(top5, transferability_all))
}else{

    report_progress$info("No article deletion")
    return(NA)
}


} else {

  report_progress$info("No article deletion")
  return(NA)


}


}


