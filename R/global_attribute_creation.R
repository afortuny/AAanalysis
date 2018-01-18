#' Global Attribute Creation.
#'
#' Take the selected attributes and identify those that explain sales. The code groups the categorical variables into pre defined segments based on the average sales per level of attribute.
#' The user puts a threshold for the variable reduction procedure which is the 5% of improvement in the MSE of the random Forest. Those variables that explain the sales and at the same time satisfy the rpedefined rules of the user
#' suceed to pass to the next phase of the attribute regression
#'
#' requires randomForest package
#'
#' @param  range_data_attrib Data frame that contains the representative assortment,
#' with ARP and attributes to be analyzed
#' @param  primary_attributes A character vector list with the attributes to be
#' analyzed (do not include ARP in that list)
#' @param  atributte_levels A numeric value indicating how many levels of each attribute the user wants to analyze. If a variable has number of levels that exceed the number set by the user then the rest of the levels are grouped as 'other'
#' @param  atributte_theshold A numeric value indicating what is the minimum percentage of importance that we use to select the most important primary attributes on the random forest
#' @param analysis_filters A numeric value indicating the criteria that define the analysis group.
#' @param  save_path An interface used for save in-memory results as files in the local file system
#' @param  saveResults A boolean to select if function saves plots and tables. The default value is TRUE.
#'
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#' @author Alan Fortuny <Alan.FortunySicart@@adidas-group.com>
#' @return Save a data frame in the Data Scientist folder with the articles and all the attributes called "Attribute Data Mapping" without the grouping
#' @return A data frame called on article level including the sales and the attributes that are relevant to explain sales, properly grouped.
#' @return Save a pdf in the Product Designer called "Primary attributes importance" with the plot of the top 15 attributes based on the explanatory power.
#' @examples
#' \dontrun{
#'#run the code for automatic price segments
#' g_attribute_data<-
#' global_attribute_creation(range_data_attrib=data_mart_assort_ARP_clean,
#' primary_attributes=primary_attributes,
#' atributte_levels=6,
#' atributte_theshold=5,
#' analysis_filters=1
#' wd=~/Projects/Assortment Apparel SS18
#' saveResults = TRUE
#'}
#'
global_attribute_creation <- function(range_data_attrib,
                                      primary_attributes,
                                      atributte_levels=8,
                                      atributte_theshold=5,
                                      analysis_filters,
                                      save_path,
                                      saveResults = TRUE)
{
  #Filter by the sales attributes of interest and the primary attributes of interest#
  range_data_attrib_all <-
    range_data_attrib[, c("Article_Number", "norm_sales", primary_attributes)]

  ## Substitute blanks by not used ##

  range_data_attrib_all <- as.data.frame(apply(range_data_attrib_all, 2, function(x)
    sub("^$", paste0("Not_Used"), x)))

  #save data for the incremental sales calculation
  if (saveResults) {
    save(range_data_attrib_all,
         file = save_path$ds_file(analysis_filters, "attribute_data_mapping", ".Rda"))
  }

  #create a copy of the table to run the attribute grouping
  range_data_attrib_grouped <- range_data_attrib_all

  #remove ARP from primary att
  ## Group levels by sales total amount of sales and pick only the most important  ##
  for (at in primary_attributes) {
    # test: at = "Col_LACE_ASSEMBLY"
    if (length(unique(range_data_attrib_all[, at])) > atributte_levels) {
      TOP_levels <-
        as.data.frame(aggregate(
          as.numeric(as.character(range_data_attrib_all[, "norm_sales"])),
          by = list(range_data_attrib_all[, at]),
          FUN = mean
        ))
      TOP_levels <- TOP_levels[order(TOP_levels[, 2], decreasing = T), ]
      TOP_levels <- TOP_levels[1:atributte_levels, ]
      levels(range_data_attrib_grouped[, at])[which(!levels(range_data_attrib_grouped[, at]) %in% as.character(TOP_levels[, 1]))] <-
        paste0("Other")

    }
  }


  ## Identify the most relevant primary attribute levels using a regression tree ##

  formula <-
    as.formula(paste0(
      "norm_sales",
      "~",
      "+",
      paste0(primary_attributes, collapse = "+")
    ))
  range_data_attrib_grouped[, "norm_sales"] <-
    as.numeric(as.character(range_data_attrib_grouped[, "norm_sales"]))
  # Prune Trees up to 8 nodes, to select only the most important of them and also disregard noise
  fit.rf = randomForest::randomForest(formula, data = range_data_attrib_grouped, maxnodes =
                                        8,na.action=na.omit)

  ## get the decrease in impurity per variable
  variable_importance <-
    as.data.frame(randomForest::importance(fit.rf))
  variable_importance[, 2] <-
    rownames(as.data.frame(randomForest::importance(fit.rf)))
  colnames(variable_importance) <-
    c("Variable_Importance", "Attributes")
  variable_importance <-
    variable_importance[order(variable_importance$Variable_Importance), ]
  variable_importance[, "Percentage"] <-
    (variable_importance[, 1] / sum(variable_importance[, 1], na.rm = TRUE)) *
    100
  variable_importance_2 <- variable_importance
  variable_importance[, "Attributes"] <-
    tolower(rownames(variable_importance))

  ## We will only plot the top 15 attributes

  if (nrow(variable_importance) > 15) {
    variable_importance <-
      variable_importance[(nrow(variable_importance) - 14):nrow(variable_importance), ]
  }


  # Turn name into a factor, with levels in the order of nameorder
  variable_importance$Attributes <-
    factor(variable_importance$Attributes, levels = variable_importance$Attributes)

  if (saveResults) {
    #Create a pdf to send the results of the primary attribute importance analysis
    pdf(save_path$pd_file(analysis_filters, "primary_attribute_weights", ".pdf"))

    #ggplot to show the relative importanc eof each primary attribute

    plot <-
      ggplot2::ggplot(variable_importance,
                      ggplot2::aes(x = variable_importance$Percentage, y = variable_importance$Attributes)) + ggplot2::geom_vline(
                        xintercept = atributte_theshold,
                        linetype = "dashed",
                        color = "red",
                        size = 0.5
                      ) +
      ggplot2::ggtitle("Primary Attribute Importance") +
      ggplot2::geom_segment(ggplot2::aes(yend = variable_importance$Attributes),
                            xend = 0,
                            colour = "grey50") +
      ggplot2::geom_point(size = 4,
                          ggplot2::aes(colour = variable_importance$Attributes),
                          show.legend = FALSE) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 15)) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour = "grey40", linetype = "dashed")
      )

    print(plot)

    dev.off()
  }


  # We select the attributes that we are going to use in the regression #
  # Attributes with less than X% of relevance will be removed from the analysis#

  variable_importance_3 <-
    subset(variable_importance_2,
           variable_importance_2$Percentage > atributte_theshold)

  # If there are not variables with enough contribution return empty list

  if (nrow(variable_importance_3)==0){
    return(NA)

  } else {

  range_data_attrib_reduc <-
    range_data_attrib_grouped[, c("Article_Number",
                                  "norm_sales",
                                  as.character(variable_importance_3[, "Attributes"]))]

  #Adjust the class of the attribute variables to character format
  for (i in 3:length(range_data_attrib_all)) {
    range_data_attrib_all[, i] <- as.character(range_data_attrib_all[, i])
  }
  #Adjust the class of the norm sales variable to numeric format
  range_data_attrib_all[, "norm_sales"] <-
    as.numeric(as.character(range_data_attrib_all[, "norm_sales"]))

  return(list(
    range_data_attrib_reduc,
    range_data_attrib_all,
    variable_importance_3
  ))
  }
}
