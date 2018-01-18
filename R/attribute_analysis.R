#' Attribute_analysis
#'
#' Generate primary weight pie plot, secondary level attribute lineplot
#'
#' requires ggplot2
#'
#' @param  models_final A data frame that contains the primary attribute weight
#' and secondary level attribute coefficients of all the models run in the regression process
#' @param  Attribute_data_mapping A data set containing the articles and attributes
#' that were analyzed in the regression, grouped accordingly
#' @param  analysis_filters A string vector indicating the criteria that define
#' the analysis group
#' @param  primary_attributes A string vector containing the list of primary
#' attributes used in the regression
#' @param  secondary_attr A string vector containing the list of secondary levels
#' tested in the regression from which there are estimated coefficients in models_final
#' @param  prim_attr_weights A numeric vector including the estimated weights of the
#' primary attributes analyzed in the regression.
#' @param  save_path An interface used for save in-memory results as files in the local file system
#' @param  saveResults A boolean to select if function creates plots and writes files
#'
#' @return A pie chart in pdf format with primary attribute weight.
#' @return Line plot indicating secondary attribute preference
#' @return Top 100 attribute combinations
#'
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#' @author Alan Fortuny <Alan.FortunySicart@@adidas-group.com>
#'
#' @examples
#' \dontrun{
#' #load data needed
#' load("data/models_final.RData")
#' load("data/g_attribute_data.RData")
#' #Indicate primary att weights based on regression results
#' prim_attr_weights=c(0.2355152,0.2222791,0.2339549,0.3082508)
#' #Run the analysis
#'attribute_analysis(models_final=models_final,
#'                   Attribute_data_mapping=g_attribute_data,
#'                   analysis_filters=1,
#'                   primary_attributes=colnames(g_attribute_data[,
#'                   which(!(colnames(g_attribute_data) %in% c("Article_Number","norm_sales")))]),
#'                   secondary_attr=names(models_final[(5+length(prim_attr_weights)+1):
#'                   ncol(models_final)]),
#'                   prim_attr_weights=c(0.2355152,0.2222791,0.2339549,0.3082508),
#'                   wd=~/Projects/Assortment Apparel SS18)
#'}
attribute_analysis <- function(models_final,
                               Attribute_data_mapping,
                               analysis_filters,
                               primary_attributes,
                               secondary_attr,
                               prim_attr_weights,
                               save_path,
                               saveResults = TRUE) {

  ## Now we plot the primary and secondary median importance plots, for the designer and merchandise manager

  # Pie Chart with primary weights
  slices <- prim_attr_weights
  pct <- round(slices / sum(slices) * 100)

  if (anyNA(pct))
    return(NA)

  primary_att_data <- as.data.frame(cbind(primary_attributes, pct))
  colnames(primary_att_data) <- c("Attribute", "Importance")


  # Chart with secondary median impact on sales
  #first create tables
  secondary_coeff <-
    apply(models_final[, secondary_attr], 2, function(x)
      stats::median(x[x > 0], na.rm = TRUE))
  secondary_coeff_c <- secondary_coeff
  for (var in 1:length(secondary_coeff)) {
    secondary_coeff[var] <-
      (secondary_coeff_c[var] / sum(secondary_coeff_c, na.rm = TRUE))
  }

  secondary_coef_table <-
    matrix(ncol = 4, nrow = length(secondary_coeff))
  colnames(secondary_coef_table) <-
    c("Primary_Coeff", "Secondary_Coeff", "Importance","Coefficient_Value")
  secondary_coef_table[, "Secondary_Coeff"] <-
    names(secondary_coeff)
  secondary_coef_table[, "Importance"] <-
    as.numeric(secondary_coeff)
  secondary_coef_table[, "Coefficient_Value"] <-
    as.numeric(secondary_coeff_c)


  ## Assign to each secondary level the primary level
  for (at in primary_attributes) {
    for (r in 1:nrow(secondary_coef_table)) {
      if (length(unlist(strsplit(
        secondary_coef_table[r, "Secondary_Coeff"], split = paste0(at, "_")
      ))) == 2) {
        secondary_coef_table[r, "Primary_Coeff"] <- at
      }
    }
  }

  secondary_coef_table <- as.data.frame(secondary_coef_table)
  secondary_coef_table[, "Importance"] <-
    as.numeric(as.character(secondary_coef_table[, "Importance"])) * 100
  secondary_coef_table[, "Secondary_Coeff"] <-
    tolower(secondary_coef_table[, "Secondary_Coeff"])
  secondary_coef_table[, "Primary_Coeff"] <-
    tolower(secondary_coef_table[, "Primary_Coeff"])

  # Turn name into a factor
  secondary_coef_table$Secondary_Coeff <-
    as.factor(secondary_coef_table$Secondary_Coeff)

  # Order by importance for each primary attribute
  secondary_coef_table <-
    secondary_coef_table[order(secondary_coef_table$Primary_Coeff,
                               secondary_coef_table$Importance), ]

  secondary_coef_table[, "Primary_Coeff"] <-
    gsub("_", " ",   secondary_coef_table[, "Primary_Coeff"])



  return(list(primary_att_data, secondary_coef_table))
}
