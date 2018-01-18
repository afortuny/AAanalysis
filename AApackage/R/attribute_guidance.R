#' Attribute Guidance
#' The following function uses a lasso regressionn from glmnet in order to select variables and estimate variable contribution to sales.
#' Based on the coefficients and the number of articles with those attributes, it gives the % of sales driven by each primary attribute, that contributes at least 1% to explian sales.
#' Note that for the top 3 primary attributes, additional columns are added with the specific figures
#'
#'
#' requires glmnet dummies
#'
#' @param  data_group Data frame that contains the range of a specific grouping, with Article_Number, norm_sales and the attributes in text format
#' @param  primary_attributes A character vector including the columns of the attributes to be analyzed
#' @param  atributte_theshold A number that indicates the minimum contribution accepted for a primary attribute to be displayed, over all sales explained by attributes
#' @param  group A text that indicates the group analyzed
#' @param  Article_id A text string indicating the column name of Article_id
#' @param  Volume A text string indicating the column name of norm_sales
#' @author Alan Fortuny <Alan.FortunySicart@@adidas-group.com>
#' @return Save a data frame that contains all the information at article level based on the Lasso regression with minimum cross validated error, save primary attribute weights
#' @export
#' @examples
#' \dontrun{
#' attribute_guidance<- function(data_group=data_group ,
#'                              primary_attributes=primary_attributes,
#'                              atributte_theshold =  1)
#'
#'}



attribute_guidance <- function(data_group ,
                               primary_attributes,
                               atributte_threshold =  1,
                               group,
                               Article_id = "Article_Number",
                               Volume = "norm_sales") {
  ### We will use lasso for estimating attribute importance and variable selection

  y<-as.numeric(as.character(data_group[, Volume]))

  ### We will identify the outliers based on the Tukey’s method to identify the outliers ranged above and below the 1.5*IQR.

  outliers_y<-boxplot.stats(y)$out

  outliers_idex<-which(y %in% outliers_y)

  if(length(outliers_idex)>0){
    y<-y[-which(1:length(y) %in% outliers_idex)]

  }





  #all variables are transformed factors
  xvar <-
    as.matrix(t(apply(data_group[, primary_attributes], 1, function(x)
      as.factor(x))))

  ### X is the matrix with dummys
  X <- c()

  ### A will hold attribute names
  A <- c()

  ### will store the coefficients of the lasso
  C_d <- c()

  for (at in primary_attributes) {
    dum <- dummies::dummy(x=at,data=xvar)
    colnames(dum) <-
      paste0(gsub(at, paste0(at, "_"), colnames(dum)))
    X <- cbind(X, dum)
    A <- rbind(A, cbind(at, colnames(dum)))
    C_d <- cbind(C_d, paste0(at, "_", xvar[, at]))
  }



  ### subset the data to train and validate the model without outliers

  if(length(outliers_idex)>0){
    X<-X[-outliers_idex,]

  }




  ### run the model for the first time
  cvfit <- glmnet::cv.glmnet(x = X,
                             y = y,
                             lower.limits = 0,alpha=0,nfolds=3)



  coef_df <- as.matrix(coef(cvfit, s = "lambda.min"))
  coef_df <- cbind(coef_df, rownames(coef_df))
  colnames(coef_df) <- c("coefficient", "variable")
  coef_df <- subset(coef_df, coef_df[, "coefficient"] > 0)
  ### re-run the model with the positive variables
  X <- X[, which(colnames(X) %in% coef_df[, "variable"])]
  cvfit <- glmnet::cv.glmnet(x = X,
                             y = y,
                             lower.limits = 0,alpha=0,nfolds=3)


  ### get the coefficients
  coef_df <- as.matrix(coef(cvfit, s = "lambda.min"))



  coef_df <- cbind(rownames(coef_df), coef_df)
  colnames(coef_df) <-
    c("secondary_attribute_name", "secondary_attribute_value")

  ### Filter out the intercept
  coef_df<-coef_df[-which(coef_df[,"secondary_attribute_name"]=="(Intercept)"),]


  ### If we do not have a single valid variable we will not report any model

  if (nrow(coef_df)>=1){



  colnames(A) <-
    c("primary_attribute_name", "secondary_attribute_name")
  ### merge it with all attributes


  attribute_guidance_df <-
    merge(coef_df,
          A,
          by = c("secondary_attribute_name"),
          all.y = TRUE)

  attribute_guidance_df[, "secondary_attribute_value"] <-as.numeric(as.character(attribute_guidance_df[, "secondary_attribute_value"]))
   attribute_guidance_df[, "secondary_attribute_value"] <-
    ifelse(is.na(attribute_guidance_df[, "secondary_attribute_value"]), 0,attribute_guidance_df[, "secondary_attribute_value"])



  attribute_guidance_df[, "secondary_attribute_value"] <-
    as.numeric(as.character(attribute_guidance_df[, "secondary_attribute_value"]))
  attribute_guidance_df[, "secondary_attribute_name"] <-
    as.character(attribute_guidance_df[, "secondary_attribute_name"])


  ## calculate primary attribute importance

  C_coeff <- matrix(ncol = 2)
  colnames(C_coeff) <- c("secondary_attribute_name", "Freq")

  for (c in 1:ncol(C_d)) {
    C_coeff_temp <- as.data.frame(table(C_d[, c]))
    colnames(C_coeff_temp) <- c("secondary_attribute_name", "Freq")
    C_coeff <- rbind(C_coeff, C_coeff_temp)

  }

  C_coeff <- C_coeff[-1, ]


  ## assign to each article the coefficient that corresponds to its attributes

  attribute_guidance_df <-
    merge(attribute_guidance_df,
          C_coeff,
          by = c("secondary_attribute_name"))

  attribute_guidance_df[, "total_secondary_contribution"] <-
    attribute_guidance_df[, "secondary_attribute_value"] * attribute_guidance_df[, "Freq"]

  ##primary_attribute_importance

  prim_attribute_importance <-
    as.data.frame(
      tapply(
        attribute_guidance_df$total_secondary_contribution,
        attribute_guidance_df$primary_attribute_name,
        FUN = sum
      )
    )
  prim_attribute_importance <-
    cbind(rownames(prim_attribute_importance),
          prim_attribute_importance)
  colnames(prim_attribute_importance) <-
    c("primary_attribute_name", "estimated_sales")

  prim_attribute_importance[, "primary_attribute_perc"] <-
    (prim_attribute_importance[, "estimated_sales"] / sum(prim_attribute_importance[, "estimated_sales"],na.rm=TRUE)) * 100
  prim_attribute_importance <-
    prim_attribute_importance[order(prim_attribute_importance[, "primary_attribute_perc"], decreasing =
                                      TRUE), ]

  ##eliminate estimated sales and total contribution

  prim_attribute_importance <-
    prim_attribute_importance[, -which(colnames(prim_attribute_importance) %in% "estimated_sales")]



   attribute_guidance_df <-
    attribute_guidance_df[, -which(colnames(attribute_guidance_df) %in% c("total_secondary_contribution", "Freq"))]

  ##pick the attribute with at least 1% of contribution to sales

  prim_attribute_importance <-
    prim_attribute_importance[which(prim_attribute_importance[, "primary_attribute_perc"] >=
                                      atributte_threshold), ]


  ## normalize to the total explanining mor ethan 1%
  prim_attribute_importance[, "primary_attribute_perc"] <-
    (prim_attribute_importance[, "primary_attribute_perc"] / sum(prim_attribute_importance[, "primary_attribute_perc"])) *100




   prim_attribute_importance[, "rank"] <-
    1:nrow(prim_attribute_importance)

  ##merge the primary attribute importance with the secondary importance coefficient

  attribute_guidance_df <-
    merge(
      attribute_guidance_df,
      prim_attribute_importance,
      by = c("primary_attribute_name")
    )

  ##merge the attribute guidance with article level information

  ## first we create the attribute mapping table for the desired primary attributes
  data_mapped_group <- c()

  for (art in 1:nrow(data_group)) {
    for (at in  as.character(unique(attribute_guidance_df[, "primary_attribute_name"]))) {
      data_mapped_temp <-
        cbind(as.character(data_group[art, Article_id]), as.character(paste0(at, "_", data_group[art, at])))
      data_mapped_group <- rbind(data_mapped_group, data_mapped_temp)
    }

  }
  colnames(data_mapped_group) <-
    c(Article_id, "secondary_attribute_name")


  #merge article mapping with the attributes

  attribute_guidance_df <-
    merge(data_mapped_group,
          attribute_guidance_df,
          by = c("secondary_attribute_name"))


  #create the top3 rank cases and the sum of the coefficients for those

  rank3 <- matrix()
  rank3_final <- matrix(ncol = 13)
  colnames(rank3_final) <-
    c(
      Article_id,
      "primary_attribute_name_rank_1",
      "primary_attribute_perc_rank_1"    ,
      "secondary_attribute_name_rank_1",
      "secondary_attribute_value_rank_1"    ,
      "primary_attribute_name_rank_2"   ,
      "primary_attribute_perc_rank_2"     ,
      "secondary_attribute_name_rank_2",
      "secondary_attribute_value_rank_2"  ,
      "primary_attribute_name_rank_3",
      "primary_attribute_perc_rank_3"      ,
      "secondary_attribute_name_rank_3",
      "secondary_attribute_value_rank_3"
    )


  for (art in as.character(data_group[, Article_id])) {
    for (rank in 1:min(max(attribute_guidance_df[, "rank"]), 3)) {
      rank3_temp <-
        attribute_guidance_df[which(attribute_guidance_df[, "rank"] == rank &
                                      attribute_guidance_df[, Article_id] == art), c(
                                        "primary_attribute_name",
                                        "primary_attribute_perc",
                                        "secondary_attribute_name",
                                        "secondary_attribute_value"
                                      )]
      colnames(rank3_temp) <-
        c(
          paste0("primary_attribute_name", "_rank_", rank),
          paste0("primary_attribute_perc", "_rank_", rank)  ,
          paste0("secondary_attribute_name", "_rank_", rank),
          paste0("secondary_attribute_value", "_rank_", rank)
        )

      rank3 <- cbind(rank3, rank3_temp)

    }
    rank3[, 1] <- art
    colnames(rank3)[1] <- Article_id
    rank3_final <- rbind(rank3_final, rank3)
    rank3 <- matrix()
  }

  rank3_final <- rank3_final[-1, ]

  attribute_guidance_df_table <-
    merge(attribute_guidance_df, rank3_final, by = c(Article_id))
  attribute_guidance_df_table[, "Grouping"] <- group

  attribute_guidance_df_table[, "Total_coefficient_attribute_top3_bundle"] <-  ifelse( is.na(attribute_guidance_df_table[, "secondary_attribute_value_rank_1"]),0,attribute_guidance_df_table[, "secondary_attribute_value_rank_1"])+
    ifelse( is.na(attribute_guidance_df_table[, "secondary_attribute_value_rank_2"]),0,attribute_guidance_df_table[, "secondary_attribute_value_rank_2"])+
    ifelse( is.na(attribute_guidance_df_table[, "secondary_attribute_value_rank_3"]),0,attribute_guidance_df_table[, "secondary_attribute_value_rank_3"])

  return(list(attribute_guidance_df_table,prim_attribute_importance))

  } else {

    return(NA)
  }

}

