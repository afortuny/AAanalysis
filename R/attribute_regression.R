#' attribute_regression
#'
#' Prepares data and run regressions for different combination of attributes, providing best out of sample regression models. Generates model stability plots and data to validate primary attribute weight estimation.
#' The code creates from thousand up to million of models to examine. The analysis runs an ANOVA Regression algorithm and promotes the models that satisfy the rules provided by the user.
#' From the suceeded models we then estimate the Primary Attribute Importance in the attribute analysis code
#'
#' requires RcppEigen, caTools, foreach and doparallel packages
#'
#' @param data_regression A data frame on article level including the article number, normalized sales, and the attributes properly grouped
#' @param analysis_filters A string vector indicating the criteria that define the analysis group
#' @param maximum_pvalue A number indicating the maximum pvalue accepted on the variables (default value is 0.5)
#' @param max_weight A number indicating the maximum of the weight of the primary attributes (calculated as the median impact of secondary levels)
#' @param models_until A number indicating the minimum number of variables to test in the regression (default value is 5)
#' @param combi A number indicating the number of variable combinations to be tested in the attribute regression (minimum value is 5)
#' @param primary_attributes A character vector indicating the primary attributes to be used for the regression
#' @param save_path An interface used for save in-memory results as files in the local file system
#' @param saveResults A boolean to select if function should return plots and tables in files.
#'
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#' @author Alan Fortuny <Alan.FortunySicart@@adidas-group.com>
#' @return A vector with the primary weights, in the same order as the primary attribute columns.
#' @return Model outputs for Data Scientist validation of model performance.
#'
#' @examples
#' \dontrun{
#' #Run the function to get the primary attribute weights
#'attribute_regression(data_regression=g_attribute_data,
#'analysis_filters=1,
#'maximum_pvalue=0.5,
#'max_weight=0.6,
#'combi=10000,
#'primary_attributes=primary_attributes
#'wd=~/Projects/Assortment Apparel SS18)
#'}

attribute_regression <- function(data_regression,
                                 analysis_filters,
                                 maximum_pvalue = 0.5,
                                 max_weight = 0.6,
                                 models_until = 5,
                                 combi = 10000,
                                 primary_attributes,
                                 save_path,
                                 saveResults = TRUE) {
  #Set generic data table name
  Adidas_data <- data_regression

  # eliminate "/" in product types
  analysis_filters = gsub("/", "_", analysis_filters , fixed = TRUE)

  #eliminate levels signalling that can unable formula running
  for (c in 1:ncol(Adidas_data)) {
    Adidas_data[, c] <- gsub(" ", "_", Adidas_data[, c] , fixed = TRUE)
    Adidas_data[, c] <- gsub(",", "_", Adidas_data[, c] , fixed = TRUE)
    Adidas_data[, c] <- gsub("]", "", Adidas_data[, c] , fixed = TRUE)
    Adidas_data[, c] <- gsub("[", "", Adidas_data[, c] , fixed = TRUE)
    Adidas_data[, c] <- gsub(" ", "_", Adidas_data[, c] , fixed = TRUE)
    Adidas_data[, c] <- gsub("(", "", Adidas_data[, c] , fixed = TRUE)
    Adidas_data[, c] <- gsub(")", "", Adidas_data[, c] , fixed = TRUE)
    Adidas_data[, c] <- gsub("/", "_", Adidas_data[, c] , fixed = TRUE)
  }

  #Attribute Reggression ###
  range = 1:length(primary_attributes)

  data = Adidas_data[, primary_attributes]

  #Paste the primary attribute to each secondary attribute to ensure uniqueness#
  for (c in 1:ncol(data)) {
    for (r in 1:nrow(data)) {
      data[r, c] <- paste0(colnames(data)[c], "_", data[r, c])
    }
  }

  #########################
  # Retrieve all possible #
  # secondary level attribute  #
  #########################
  primary_attr_len = c()
  secondary_attr = c()
  for (i in 1:ncol(data)) {
    attr = unique(as.character(data[, i]))
    primary_attr_len = c(primary_attr_len, length(attr))
    secondary_attr = c(secondary_attr, attr)
  }

  #################################################################
  # Prepare data mat with secondary attributes as dummy variables #
  #################################################################
  data_mat = matrix(0, nrow = nrow(data), ncol = length(secondary_attr))

  colnames(data_mat) = secondary_attr
  for (i in 1:nrow(data)) {
    data_mat[i, which(colnames(data_mat) %in% data[i, ])] = 1
  }

  # append sales
  data_mat = as.data.frame(cbind(as.numeric(Adidas_data[, "norm_sales"]), data_mat))
  colnames(data_mat)[1] = "y_norm"

  # Iterative Procedure
  coeff = secondary_attr	# select all secondary from existing dataset

  #Create subset of all possible combinations x 2^(#var) above a specific number
  #Since the exhaustive number of combs cannot be created because of its final sizze we create a subset of those combs with a sampling without replacement
  tt <- matrix(nrow = 0, ncol = length(coeff))

  for (i in length(coeff):models_until) {
    if (length(coeff) == i) {
      tt1 <- caTools::combs(v = coeff, k = i)
      tt <- rbind(tt, tt1)
    } else {
      set.seed(123)
      tt1 <-
        t(replicate(min(combi, choose(
          length(coeff), i
        )), sample(coeff, i, replace = FALSE)))
      tt1 <- t(apply(tt1, 1, sort))
      tt1 <- tt1[!duplicated(tt1), ]
      tt1 <-
        cbind(tt1, matrix(nrow = nrow(tt1), ncol = length(coeff) - i))
      tt <- rbind(tt, tt1)
    }
  }

  tt2 <- tt

  # Contrained least squares - learn the model via exhaustive search
  anova_regression <- function(i, tt2, data_mat) {
    model_output_R <-
      as.data.frame(matrix(
        nrow = 1,
        ncol = 6 + length(range) + length(coeff)
      ))
    colnames(model_output_R) <-
      c(
        "Model_Success",
        "Rsquared_adj",
        "MSE_train",
        "MSE_val",
        "Max_pvalue",
        "Mean_pvalue",
        primary_attributes,
        coeff
      )

    #select the variables
    var_list = unique(na.omit(unlist(tt2[i, ])))

    set.seed(123)
    #select the training sample and the validation sample
    in_sample_index <-
      sample(1:nrow(data_mat), round(0.75 * nrow(data_mat), 0), replace = FALSE)

    #define the variables
    X <- as.matrix(data_mat[in_sample_index, var_list])
    y <- as.matrix(data_mat[in_sample_index, "y_norm"])

    #run the regression, fastLm is a simpler implementation of lm with lower computation time
    fit <- RcppEigen::fastLm(y = y, X = X)
    coef_table <- as.data.frame(fit$coefficients)

    #all coefficients must be positive and non na
    if (min(fit$coefficients, na.rm = TRUE) >= 0 &
        length(fit$coefficients) > 0) {
      pvalue <- as.data.frame(summary(fit)$coefficients)[, 4]

      #pvalue must be finite and below maximum threshold
      if (max(pvalue, na.rm = TRUE) <= maximum_pvalue &
          is.finite(max(pvalue, na.rm = TRUE))) {
        primary_attr = colnames(Adidas_data[, primary_attributes])
        primary_coeff = matrix(0, nrow = length(primary_attr), ncol = 1)
        rownames(primary_coeff) = primary_attr

        #calculate the primary attribute weight based on median coefficient of secondary levels
        for (j in 1:ncol(Adidas_data[, primary_attributes])) {
          indx = which(names(fit$coefficients) %in% data[, j])
          primary_coeff[j, 1] = median(fit$coefficients[indx], na.rm = TRUE)

        }

        res = primary_coeff / sum(primary_coeff, na.rm = TRUE) #assign the attribute importance

        ## get the weights of those from which the maximum weight is below the threshold ##
        if (max(res, na.rm = TRUE) < max_weight) {
          model_output_R[1, "Model_Success"] <- i #assign  the succeeded model
          model_output_R[1, colnames(Adidas_data[, primary_attributes])] <-
            res[, 1] #assign the attribute importance
          model_output_R[1, "Rsquared_adj"] = summary(fit)$adj.r.squared #assign the R Square
          model_output_R[1, "MSE_train"] = mean(fit$residuals ^ 2, na.rm =
                                                  TRUE) #Assign the MSE Train
          model_output_R[1, "MSE_val"] = mean((as.data.frame(
            apply(data_mat[!(1:nrow(data_mat) %in% in_sample_index), which(colnames(data_mat) %in% rownames(coef_table))] *
                    coef_table[, 1], 1, function(x)
                      sum(x, na.rm = TRUE))
          ) -
            data_mat[!(1:nrow(data_mat) %in% in_sample_index), "y_norm"]) ^
            2, na.rm = TRUE) #Assign the MSE Test
          model_output_R[1, "Max_pvalue"] = max(pvalue, na.rm = TRUE) # Assign the maximum p-value
          model_output_R[1, "Mean_pvalue"] = mean(pvalue, na.rm = TRUE) #assign the mean p-value
          model_output_R[1, sort(colnames(model_output_R)[which(colnames(model_output_R) %in% names(fit$coefficients))])] =
            fit$coefficients #assign the beta coefficients

          return(model_output_R)

        }
      }
    }
  }

  # Parallelize the Anova Regression procedure using the available cores
  # Foreach statement is used, %dopar% is also used to split the work per cluster, we also register the cores
  # Results are gathered and appended into one single table called model_output_R_reduced
  # We stop the clusters to release the resources
  cl <- parallel::makeCluster(parallel::detectCores() - 30)
  doParallel::registerDoParallel(cl)
  model_output_R_reduced <-
    foreach::foreach(
      i = dim(tt2)[1]:1,
      .combine = 'rbind',
      .packages = c('RcppEigen', 'caTools')
    ) %dopar% {
      anova_regression(i, tt2, data_mat)
    }
  parallel::stopCluster(cl)

  # not a valid model
  if (is.null(model_output_R_reduced))
    return(NA)

  ##Validate that no negative coefficients, nor pvalue above the limit, nor R-Square < 0 ##
  model_output_R_reduced <-
    model_output_R_reduced[!apply(model_output_R_reduced, 1, function(x) {
      any(x < 0, na.rm = TRUE)
    }), ]

  # invalid model
  if (nrow(model_output_R_reduced) < 1)
    return(NA)

  ## Order models based on validation sample MSE ##
  model_output_R_reduced <-
    model_output_R_reduced[order(model_output_R_reduced$MSE_val), ]

  #Save all models#
  if (saveResults) {
    save(model_output_R_reduced, file = save_path$ds_file(analysis_filters, "model_output_R", ".Rda"))
  }

  #Save top models top 10% on out of sample NMSE when sample size is above 300#
  if (0.1 * nrow(model_output_R_reduced) < 300) {
    models_final <-
      model_output_R_reduced[1:min(300, nrow(model_output_R_reduced)),]
  } else {
    models_final <-
      model_output_R_reduced[1:round(0.10 * nrow(model_output_R_reduced), 0), ]
  }

  models_final[is.na(models_final)] <- 0

  if (saveResults) {
    save(models_final, file = save_path$ds_file(analysis_filters, "TOP10%_model_output_R", ".Rda"))

    ##Save primary attribute importance of the ones based on regression results#
    pdf(save_path$ds_file(analysis_filters, "model_stability", ".pdf"))

    boxplot(
      models_final[, primary_attributes] * 100,
      main = "Primary Attributes Importance",
      names = tolower(primary_attributes),
      las = 2,
      par(mar = c(12, 5, 4, 2) + 0.1),
      cex.axis = 0.7
    )

    ##boxplot of secondary level coefficient(in the regression)

    ## Note that only the most important 15th secondary level attributes will be displayed in the boxplot
    if (length(coeff) > 15) {
      coeff_imp <-
        as.data.frame(apply(models_final[, coeff], 2, function(x)
          median(x[x > 0], na.rm = TRUE)))
      coeff_imp[, "Coeff"] <- rownames(coeff_imp)
      colnames(coeff_imp) <- c("Median", "Coeff")
      coeff_imp <- coeff_imp[order(-coeff_imp$Median), ]
      coeff2 <- coeff_imp[complete.cases(coeff_imp["Median"]), ]
      coeff2 <- coeff2[1:(min(15, nrow(coeff2))), "Coeff"]
    } else {
      coeff2 <- coeff
    }

    boxplot(
      models_final[, coeff2],
      las = 2,
      par(mar = c(12, 5, 4, 2) + 0.1),
      cex.axis = 0.5,
      names = tolower(names(models_final[, coeff2])),
      main = "Secondary Attributes Mean Impact on Sales"
    )

    dev.off()
  }

  ### Calculate median weight of primary weights as an input to the assortment optimization
  primary_weights <-
    as.numeric(apply(models_final[, primary_attributes], 2, function(x)
      mean(x[x > 0], na.rm = TRUE)))

  ### round it to 100, since it is based on medians do not sum up 100 ###
  attr_weights <- primary_weights / sum(primary_weights)

  return(list(attr_weights, models_final))
}
