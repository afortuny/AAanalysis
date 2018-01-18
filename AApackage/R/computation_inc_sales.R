#' Computation of Incremental Sales
#'
#' A function that computes the incremental sales per article based on selected attribute weights and gives the incremental sales of all articles, allowing us to pick the one with lowest total incremenatl sales contribution
#'
#'
#' @param raw_range_data_att A data frame with article number, normalized sales and attributes without grouping (used in the regression). This is usually from the output of function  Global attribute creation, the saved "Attribute_data_mapping....." Rda file in data scientist folder.
#' @param prim_attr_weights  A numeric vector of the primary weight importance, same order as they appear in raw_range_data_att
#' @param primary_attributes  A character vector indicating the columns sused as attributes
#' @param analysis_filters  A string vector indicating the criteria that define the analysis group
#' @param  Article_id A text string indicating the column name of Article_id
#' @param  Volume A text string indicating the column name of norm_sales
#' @param save_path An interface used for save in-memory results as files in the local file system
#' @param saveResults A boolean to select if functions ouputs plot results
#'
#' @author Alan Fortuny <Alan.FortunySicart@@adidas-group.com>
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#' @return An data frame with the incremental sales and other relevant calculations of the incrementality
#' @export
#' @examples
#' \dontrun{
#'computation_inc_sales(raw_range_data_att=raw_range_data_att,
#'prim_attr_weights=c(0.2355152,0.2222791,0.2339549,0.3082508),
#'analysis_filters=1,
#'wd=~/Projects/Assortment Apparel SS18,
#'saveResults = TRUE)
#'}
#'
computation_inc_sales <-
  function(raw_range_data_att = raw_range_data_att,
           prim_attr_weights,
           primary_attributes,
           analysis_filters,
           save_path,
           saveResults = TRUE,
           Article_id = "Article_Number",
           Volume = "norm_sales") {

    #data contains the data with the article,norm_sales,main attributes
    data <- raw_range_data_att

    #eliminate problematic  symbols
    for (c in primary_attributes) {
      data[, c] <- gsub(" ", "_", data[, c] , fixed = TRUE)
      data[, c] <- gsub(",", "_", data[, c] , fixed = TRUE)
      data[, c] <- gsub("]", "", data[, c] , fixed = TRUE)
      data[, c] <- gsub("[", "", data[, c] , fixed = TRUE)
      data[, c] <- gsub(" ", "_", data[, c] , fixed = TRUE)
      data[, c] <- gsub("(", "", data[, c] , fixed = TRUE)
      data[, c] <- gsub(")", "", data[, c] , fixed = TRUE)
      data[, c] <- gsub("/", "_", data[, c] , fixed = TRUE)

    }

    #make sure colum names are unique by merging
    for (c in primary_attributes) {
      for (r in 1:nrow(data)) {
        data[r, c] <- paste0(c, "_", data[r, c])
      }
    }

    #convert the norm_sales into numeric
    data[,Volume] <- as.numeric(as.character(data[,Volume]))

    # Load Data Primary attributes coefs (the normalized sales must be at the second column and the article at the first column)
    Attributes = primary_attributes
    coef_Reg = matrix(data = prim_attr_weights,
                      nrow = 1,
                      ncol = length(Attributes))

    colnames(coef_Reg) = Attributes



    # Merge the coefficients from attribute regression with sales dataset
    data = cbind(data[,c(Article_id,Volume,Attributes)], t(matrix(
      rep(coef_Reg, ncol = length(coef_Reg)),
      ncol = dim(data)[1],
      nrow = length(coef_Reg)
    )))

    # name the columns with attribute weights
    colnames(data) = c(Article_id, Volume,
                       Attributes,
                       paste("Coef", Attributes, sep = ""))

    # calculate sales for each primary attributes
    data = cbind(data, as.numeric(data[, Volume]) * t(matrix(
      rep(coef_Reg, ncol = length(coef_Reg)),
      ncol = dim(data)[1],
      nrow = length(coef_Reg)
    )))

    # add names
    colnames(data) = c(
      Article_id,
      Volume,
      Attributes,
      paste("Coef", Attributes, sep = ""),
      paste("Vol", Attributes, sep = "")
    )


    # norm_sales is totaled by attribute value
    #tot_vol = sum(as.numeric(data[, Volume]))
    tot_vol<-c()
    for (i in 1:length(coef_Reg)) {
      tot_vol = rbind(tot_vol, aggregate(
        as.numeric(data[, (dim(data)[2] - length(coef_Reg) + i)]),
        by = list(data[, 2 + i]) ,
        FUN = sum
      ))
    }

    # Item Attribute Incrementality Coefficient table
    Attr_Incrementality = matrix(0, nrow = dim(data)[1], ncol = length(coef_Reg) +
                                   1)
    colnames(Attr_Incrementality) = c(Article_id, Attributes)

    # Calculation of Incremental coefficients
    for (i in 1:dim(Attr_Incrementality)[1]) {
      Attr_Incrementality[i, 1] = data[i, 1]
      for (j in 1:length(coef_Reg)) {
        Attr_Incrementality[i, j + 1] = (as.numeric(data[i, dim(data)[2] -
                                                               length(coef_Reg) + j]) ^ 2 /
                                           tot_vol[which(tot_vol[, 1] == data[i, colnames(coef_Reg)[j]]), 2] ^
                                           2)
      }
    }



    # norm_sales loss/retained
    vol_lost <- apply(Attr_Incrementality[, seq(2, dim(Attr_Incrementality)[2], 1)], 2, as.numeric) *
      apply(data[, seq(dim(data)[2] - length(coef_Reg) + 1, dim(data)[2], 1)], 2, as.numeric)

    colnames(vol_lost)<-paste("vol_lost_", Attributes, sep = "")


    vol_transferred <- apply(data[, seq(dim(data)[2] - length(coef_Reg) +
                                          1, dim(data)[2], 1)], 2, as.numeric) - vol_lost


    colnames(vol_transferred)<-paste("vol_transferred_", Attributes, sep = "")



    # add norm_sales lost per attribute
    data<- as.data.frame(cbind(data, vol_lost,vol_transferred))

    # add incremental sales as the total sales lost if article is deleted
    data[,"inc_sales"]<-rowSums(data[, colnames(vol_lost)])


    data <- data[order(data[, "inc_sales"], decreasing = TRUE), ]
    data[, "inc_sales_cum_sum"] <- cumsum(data[, "inc_sales"])
    data[, "inc_sales_cum_sum_perc"] <-
      (data[, "inc_sales_cum_sum"] / sum(data[, "inc_sales"])) * 100



    return(data)

  }
