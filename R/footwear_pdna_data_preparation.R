#' Footwear PDNA data preparation.
#'
#' Creates the Color & Material values for each Location of a Footwear. I slao pulls the rest of the attributes from the BDP for the footwear articles
#'
#' requires reshape2 & sqldf package
#'
#' @param  technical_settings The required parameters to set for Spark
#' @param  analytical_settings The required parameters for the assortment analysis
#' @param  sc is the established connection of Spark created by the user
#' @param  scope the scope or work (article + grouping)
#'
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#' @return Save a list of data frames that are (a) the consolidated Color & Material values for each location of a footwear per article in scope and (b) the rest of the attributes from the BDP for the articles such as gender, age, business unit and so on.
#' @examples
#' \dontrun{
#'footwear_pdna_data_preparation<-function(analytical_settings,
#'                                           technical_settings)
#'}
#'
footwear_pdna_data_preparation <- function(analytical_settings,
                                           technical_settings,
                                           sc,
                                           scope) {
  #PDNA data consolidation.
  #Pull data from HUE
  #do renames to initial variables
  #keep the main parts of each location of a footwear
  #keep the locations of each
  #Pull final data set from Spark
  attributes <- pdna_consolidation(technical_settings,
                                   analytical_settings,
                                   sc,
                                   scope)

  ## Color Preparation
  #Prepare color data by keeping the most important variables
  colors_initial <- sqldf::sqldf(
    "select article_no,
    season*1.0000 as season,
    part_name_loc,
    part_no_other,
    part_name,
    color
    from attributes
    order by article_no,
    season desc "
  )

  #Find the latest season of a Footwear with the latest color characteristics
  max_season_of_article <- sqldf::sqldf("select article_no,
                                 max(season) as season
                                 from colors_initial
                                 group by article_no")

  #Keep the latest color characteristics of a footwear. Those of the latest season stored in the PDNA data
  colors_initial <- sqldf::sqldf(
    "select a.*,
    case when a.season = b.season then 1 else 0 end as rnk
    from colors_initial as a
    left join max_season_of_article as b
    on a.article_no = b.article_no
    where rnk = 1
    order by a.article_no,
    b.season desc"
  )

  #Keep all the unique main parts of every location per article either they have a color description recorded or the color description is missing
  #Some main parts may have a color description. Ohter may have a missing value (meaning that this part is not used for an article)
  #Other parts may have a value "Not Defined" if the part has a color but the color decription is not known.
  color_desc_of_main_parts <- sqldf::sqldf("select distinct article_no,
                                    part_no_other,
                                    color
                                    from colors_initial ")

  #Transpose the table of Color description for each part of an article
  #Each part is set as column and the color description are set as value within each cell of the table
  #Probable warnings may pop up. This is due to the fact a specific part may have more than one values for color due to the fact that we hve records of the same part from different factories
  #We keep the first record that we notice in the data
  main_parts_color <-
    reshape(
      color_desc_of_main_parts,
      idvar = "article_no",
      timevar = "part_no_other",
      direction = "wide"
    )
  main_parts_color[is.na(main_parts_color)] = ''

  #For the color table change every prerix "color." that currently has iwth the word "part".
  #This is because later we are going to match each part_name with the location id that belongs to consolidate the colors
  #The "color." string consist of 6 characters, this is why we put number 7
  #Because we change the "color." with "part" and then we concatenate with the rest of the column name
  for (i in 2:length(main_parts_color)) {
    colnames(main_parts_color)[i] <-
      paste("part", substr(colnames(main_parts_color)[i], 7, nchar(as.character(
        colnames(main_parts_color)[i]
      ))), sep = "_")
  }

  #Identify to which Location id each part belongs to
  Location <- sqldf::sqldf(
    "select distinct
    part_no_other as PART_NO_OTHER
    ,part_name as PART_NAME
    ,part_name_loc as PART_NAME_LOC
    from colors_initial
    order by PART_NAME_LOC"
  )

  #Proceed with a quality analysis to all part names for each location
  #Examine the presense of missing values and 'Not Defined' values for each part of a footwear
  #Create 3 columns. The first column includes the name of each part examined. The second column includes the count of values that are not missing and third column the count of values that are Not Defined

  main_parts_color_v2 <- sqldf::sqldf("select * from main_parts_color ")
  quality_analysis_color <- c()
  for (i in 1:length(main_parts_color_v2)) {
    if ((length(which(!is.na(
      main_parts_color_v2[, c(i)]
    ))) > 0) &
    (length(which(main_parts_color_v2[, c(i)] != '')) > 0)) {
      inter1 = cbind(colnames(main_parts_color_v2[i]),
                     as.numeric(length(which(
                       main_parts_color_v2[, c(i)] != ''
                     ))),
                     as.numeric(length(
                       which(main_parts_color_v2[, c(i)] == 'Not Defined')
                     )))
      quality_analysis_color = rbind(quality_analysis_color, inter1)
    }

  }

  quality_analysis_color <- as.data.frame(quality_analysis_color)
  quality_analysis_color[, 1] <-
    as.character(quality_analysis_color$V1)
  quality_analysis_color[, 2] <-
    as.numeric(as.character(quality_analysis_color$V2))
  quality_analysis_color[, 3] <-
    as.numeric(as.character(quality_analysis_color$V3))

  colnames(quality_analysis_color)[1:3] = c("Var_Names",
                                            "Counts_without_missing",
                                            "Counts_with_Not_Defined")

  #Exclude any parts that all their values are either missing or Not Defined
  #Exclude any parts related to Box Assembly, Packaging and Costing Sundries
  quality_analysis_color_v2 <-
    sqldf::sqldf(
      "select *,Counts_without_missing - Counts_with_Not_Defined as Valid_Counts
      from quality_analysis_color
      where Var_Names not like 'part_891%'
      and
      Var_Names not in ('part_9020','part_9025','part_9030','part_9035','part_9040','part_9045','part_9050','part_9051','part_9055','part_9060','part_9130',
      'part_9135','part_9140','part_9150','part_9170','part_9175','part_9180','part_9190','part_9200','part_9205','part_9210')
      group by Var_Names
      having Valid_Counts <> 0
      order by Valid_Counts desc"
    )

  #Keep only the parts of a footwear with at least one valid record
  col_list = main_parts_color_v2[, which(colnames(main_parts_color_v2) %in% quality_analysis_color_v2[, 1])]

  #Find all Unique Color values within all the part ids with at least one valid record
  unique_color_values <- c()
  for (j in 2:dim(col_list)[2]) {
    y = as.data.frame(unique(col_list[, j]))
    unique_color_values <- rbind(unique_color_values, y)
  }
  colnames(unique_color_values)[1] = c("Color_Names")
  unique_color_values <- sqldf::sqldf(
    "select distinct Color_Names
    from unique_color_values
    where Color_Names not in ('','Not Defined') "
  )

  length(unique(Location[, 3]))
  # Create a sequential index for each Location of the Footwear
  # For example 1 for BOX ASSEMBLY, 2 from COLAR ASSEMBLY e.t.c.
  Location_seq_index <-
    cbind(1:length(unique(Location[, 3])), as.data.frame(unique(Location[, 3])))
  colnames(Location_seq_index) <- c("counts", "PART_NAME_LOC")

  #Prepare Location Hierarchy Table
  #This table includes all available Locations of a Footwear and the respective main parts of them
  #This table includes the hierarchy of each Location and the repsective main parts of them
  #For each article in scope we will create the combination of colors found for all the main parts that constitute every location
  #Thus if all parts of a location are missing we will use "Not Used" for this location. If all parts are missing exept one that has a value Not Defined then the ocation is characterized as Not Defined
  #If only one part a color value we keep this color record for the location. If more than one parts have color values then we keep the combination of colors as value for this location
  Location_hierarchy <- sqldf::sqldf(
    "select a.*,
    b.counts
    from Location as a
    left join Location_seq_index as b
    on a.PART_NAME_LOC = b.PART_NAME_LOC"
  )

  #Create the Color Consolidation data Set
  consolidate_color <- function(Location1, p) {
    col_list4 <- c()
    #Isolate each location id and its repsective main parts
    location_id <-
      Location_hierarchy[which(Location_hierarchy$counts == p), ]
    #Use "part" as prefix to match the part name of the Location hierarchy table with the available part names found in the articles in scope
    location_id$part_name <-
      paste('part', location_id[, 1], sep = "_")
    #Isolate those part names from the articles in scope that refer to the repspective each time location
    col_list2 = as.data.frame(col_list[, which(colnames(col_list) %in% location_id[, 5])])

    # Procedure when more than one part names consist the examined location
    if (length(col_list2) > 1) {
      # Change all factor variables to character
      col_list3 <- col_list2
      isF <- sapply(col_list3, is.factor)
      col_list3[isF] <- lapply(col_list3[isF], as.character)
      sapply(col_list3, class)

      #omit any NAs and blanks as color values
      col_unique_vars <-
        matrix(NA, nrow = dim(col_list3)[1], ncol = 1)
      for (i in 1:dim(col_list3)[1]) {
        unique_var <- unique(as.character(col_list3[i,]))
        unique_var[unique_var == ""] <- NA
        unique_var[unique_var == " "] <- NA
        unique_var <- as.character(na.omit(unique_var))

        # omit any Not Defined value when another valid color value is already recorded
        if (length(unique_var) > 1) {
          remove <- 'Not Defined'
          unique_var <- unique_var[!unique_var %in% remove]
        }

        #Distinguish color combinations with "_". For example Black_White, or Grey_Yellow
        unique_var_df <- as.data.frame(unique_var)
        unique_var_2 <-
          as.character(t(sqldf::sqldf(
            "select * from unique_var_df order by unique_var"
          )))
        col_unique_vars[i,] <- paste(unique_var_2, collapse = "_")
      }
      col_unique_vars2 <- as.data.frame(col_unique_vars)
      #If the value is missing then use the value 'Not Used' instead or else use the recorded value even the Not Defined
      col_unique_vars2 <- sqldf::sqldf(
        sprintf(
          "select V1,
          case when V1='' then 'Not Used_col_%s' else V1||'_'||'col_%s' end as V1_1
          from col_unique_vars2",
          p,
          p,
          p
        )
      )
      col_list4 <- cbind(col_list4, col_unique_vars2[, c(2)])
      colnames(col_list4)[dim(col_list4)[2]] <-
        paste('Col', c(paste(unique(location_id[, 3]))), sep = '_')
    }

    # Procedure when only one part name consist the examined location
    if (length(col_list2) == 1) {
      colnames(col_list2) <- c("y")
      #If the value is missing then use the value 'Not Used' instead or else use the recorded value even the Not Defined
      col_list3 <- sqldf::sqldf(
        sprintf(
          "select
          case when y='' then 'Not Used_col_%s'
          when y='Not Defined' then 'Not Defined_col_%s'
          else y||'_'||'col_%s'
          end as x
          from col_list2",
          p,
          p,
          p
        )
      )
      col_list4 <- cbind(col_list4, col_list3[, c(1)])
      colnames(col_list4)[dim(col_list4)[2]] <-
        paste('Col', c(paste(unique(location_id[, 3]))), sep = '_')
    }

    return(col_list4)
  }

  #Detect Cores
  cl <- parallel::makeCluster(parallel::detectCores() - 30)
  doParallel::registerDoParallel(cl)
  #Create consolidated color dataset with attributes
  col_list4 <-
    foreach::foreach(
      p = 1:length(unique(Location[, 3])),
      .combine = "cbind",
      .packages = "sqldf"
    ) %dopar% {
      consolidate_color(Location1, p)
    }
  parallel::stopCluster(cl)

  #Keep the latest material characteristics of a footwear. Those of the latest season stored in the PDNA data
  materials_initial <- sqldf::sqldf(
    "select article_no,
    season*1.0000 as season,
    part_name_loc,
    part_no_other,
    part_name,
    material
    from attributes
    order by article_no, season desc "
  )

  #Find the latest season of a Footwear with the latest material characteristics
  max_season_of_article <- sqldf::sqldf("select article_no,
                                 max(season) as season
                                 from materials_initial
                                 group by article_no")


  materials_initial <- sqldf::sqldf(
    "select a.*,
    case when a.season = b.season then 1 else 0 end as rnk
    from materials_initial as a
    left join max_season_of_article as b
    on a.article_no = b.article_no
    where rnk = 1
    order by a.article_no,
    b.season desc"
  )

  #Keep all the unique main parts of every location per article either they have a material description recorded or the material description is missing
  material_desc_of_main_parts <- sqldf::sqldf("select distinct article_no,
                                       part_no_other,
                                       material
                                       from materials_initial")

  #Transpose the table of Material description for each part of an article
  #Each part is set as column and the material description are set as value within each cell of the table
  #Probable warnings may pop up. This is due to the fact a specific part may have more than one values for material due to the fact that we hve records of the same part from different factories
  #We keep the first record that we notice in the data
  main_parts_material <-
    reshape(
      material_desc_of_main_parts,
      idvar = "article_no",
      timevar = "part_no_other",
      direction = "wide"
    )
  main_parts_material[is.na(main_parts_material)] = ''

  #For the material table change every prerix "material." that currently has iwth the word "part".
  #This is because later we are going to match each part_name with the location id that belongs to consolidate the materials
  #The "material." string consist of 9 characters, this is why we put number 7
  #Because we change the "material." with "part" and then we concatenate with the rest of the column name
  for (i in 2:length(main_parts_material)) {
    colnames(main_parts_material)[i] <-
      paste("part", substr(colnames(main_parts_material)[i], 10, nchar(as.character(
        colnames(main_parts_material)[i]
      ))), sep = "_")
  }

  #Proceed with a quality analysis to all part names for each location
  #Examine the presense of missing values and 'Not Defined' values for each part of a footwear
  #Create 3 columns. The first column includes the name of each part examined. The second column includes the count of values that are not missing and third column the count of values that are Not Defined

  main_parts_material_v2 <-
    sqldf::sqldf("select * from main_parts_material ")
  quality_analysis_material <- c()
  for (i in 1:length(main_parts_material_v2)) {
    if ((length(which(!is.na(
      main_parts_material_v2[, c(i)]
    ))) > 0) &
    (length(which(main_parts_material_v2[, c(i)] != '')) > 0)) {
      x1 = cbind(
        colnames(main_parts_material_v2[i]),
        as.numeric(length(
          which(main_parts_material_v2[, c(i)] != '')
        )),
        as.numeric(length(
          which(main_parts_material_v2[, c(i)] == 'Not Defined')
        ))
      )
      quality_analysis_material = rbind(quality_analysis_material, x1)
    }

  }

  quality_analysis_material <-
    as.data.frame(quality_analysis_material)
  quality_analysis_material[, 1] <-
    as.character(quality_analysis_material$V1)
  quality_analysis_material[, 2] <-
    as.numeric(as.character(quality_analysis_material$V2))
  quality_analysis_material[, 3] <-
    as.numeric(as.character(quality_analysis_material$V3))

  colnames(quality_analysis_material)[1:3] = c("Var_Names",
                                               "Counts_without_missing",
                                               "Counts_with_Not_Defined")

  #Exclude any parts that all their values are either missing or Not Defined
  #Exclude any parts related to Box Assembly, Packaging and Costing Sundries
  quality_analysis_material_v2 <-
    sqldf::sqldf(
      "select *,Counts_without_missing - Counts_with_Not_Defined as Valid_Counts
      from quality_analysis_material
      where Var_Names not like 'part_891%'
      and
      Var_Names not in ('part_9020','part_9025','part_9030','part_9035','part_9040','part_9045','part_9050','part_9051','part_9055','part_9060','part_9130',
      'part_9135','part_9140','part_9150','part_9170','part_9175','part_9180','part_9190','part_9200','part_9205','part_9210')
      group by Var_Names
      having Valid_Counts <> 0
      order by Valid_Counts desc"
    )

  #Keep only the parts of a footwear with at least one valid record
  material_list = main_parts_material_v2[, which(colnames(main_parts_material_v2) %in% quality_analysis_material_v2[, 1])]


  #Find all Unique Material values within all the part ids with at least one valid record
  unique_material_values <- c()
  for (j in 2:dim(material_list)[2]) {
    y = as.data.frame(unique(material_list[, j]))
    unique_material_values <- rbind(unique_material_values, y)
  }
  colnames(unique_material_values)[1] = c("Names")
  unique_values <-
    sqldf::sqldf(
      "select distinct Names from unique_material_values
      where Names not in ('','Not Defined') "
    )

  #Prepare Location Hierarchy Table
  #This table includes all available Locations of a Footwear and the respective main parts of them
  #This table includes the hierarchy of each Location and the respective main parts of them
  #For each article in scope we will create the combination of materials found for all the main parts that constitute every location
  #Thus if all parts of a location are missing we will use "Not Used" for this location. If all parts are missing exept one that has a value Not Defined then the location is characterized as Not Defined
  #If only one part has a material value we keep this material value as record for the location. If more than one parts have material values then we keep the combination of materials as value for this location
  Location_seq_index <-
    cbind(1:length(unique(Location[, 3])), as.data.frame(unique(Location[, 3])))
  colnames(Location_seq_index) <- c("counts", "PART_NAME_LOC")

  Location_hierarchy <- sqldf::sqldf(
    "select a.*,
    b.counts
    from Location as a
    left join Location_seq_index as b
    on a.PART_NAME_LOC = b.PART_NAME_LOC"
  )

  #Create the Material Consolidation data Set
  consolidate_material <- function(Location1, p) {
    material_list4 <- c()
    #Isolate each location id and its repsective main parts
    location_id <-
      Location_hierarchy[which(Location_hierarchy$counts == p), ]
    #Use "part" as prefix to match the part name of the Location hierarchy table with the available part names found in the articles in scope
    location_id$part_name <-
      paste('part', location_id[, 1], sep = "_")
    #Isolate those part names from the articles in scope that refer to the repspective each time location
    material_list2 = as.data.frame(material_list[, which(colnames(material_list) %in% location_id[, 5])])
    # Procedure when more than one part names consist the examined location
    if (length(material_list2) > 1) {
      # Change all factor variables to character
      material_list3 <- material_list2
      isF <- sapply(material_list3, is.factor)
      material_list3[isF] <-
        lapply(material_list3[isF], as.character)
      sapply(material_list3, class)

      #omit any NAs and blanks as records
      mat_unique_vars <-
        matrix(NA, nrow = dim(material_list3)[1], ncol = 1)
      for (i in 1:dim(material_list3)[1]) {
        unique_var <- unique(as.character(material_list3[i,]))
        unique_var[unique_var == ""] <- NA
        unique_var[unique_var == " "] <- NA
        unique_var <- as.character(na.omit(unique_var))
        unique_var_df <- as.data.frame(unique_var)
        unique_var_2 <-
          as.character(t(sqldf::sqldf(
            "select * from unique_var_df order by unique_var"
          )))
        mat_unique_vars[i,] <- paste(unique_var_2, collapse = "_")
      }
      mat_unique_vars2 <- as.data.frame(mat_unique_vars)
      mat_unique_vars2 <-
        sqldf::sqldf(
          sprintf(
            "select V1, case when V1='' then 'Not Used_mat_%s' else V1||'_'||'mat_%s' end as V1_1
            from mat_unique_vars2",
            p,
            p,
            p
          )
        )
      material_list4 <-
        cbind(material_list4, mat_unique_vars2[, c(2)])
      colnames(material_list4)[dim(material_list4)[2]] <-
        paste('Mat', c(paste(unique(location_id[, 3]))), sep = '_')

    }

    # Procedure when only one part name consist the examined location
    if (length(material_list2) == 1) {
      colnames(material_list2) <- c("y")
      material_list3 <- sqldf::sqldf(
        sprintf(
          "select
          case when y='' then 'Not Used_mat_%s'
          when y='Not Defined' then 'Not Defined_mat_%s'
          else y||'_'||'mat_%s'
          end as x
          from material_list2",
          p,
          p,
          p
        )
      )
      material_list4 <-
        cbind(material_list4, material_list3[, c(1)])
      colnames(material_list4)[dim(material_list4)[2]] <-
        paste('Mat', c(paste(unique(location_id[, 3]))), sep = '_')
    }

    return(material_list4)

  }

  #Detect Cores
  cl <- parallel::makeCluster(parallel::detectCores() - 30)
  doParallel::registerDoParallel(cl)
  #Create consolidated material dataset with attributes
  material_list4 <-
    foreach::foreach(
      p = 1:length(unique(Location[, 3])),
      .combine = "cbind",
      .packages = "sqldf"
    ) %dopar% {
      consolidate_material(Location1, p)
    }
  parallel::stopCluster(cl)

  # Consolidate Attribute Data
  col_list5 <- as.data.frame(col_list4)
  col_list6 <-
    as.data.frame(cbind(Article = main_parts_color[,]$article_no, col_list5))


  material_list5 <- as.data.frame(material_list4)
  material_list6 <-
    as.data.frame(cbind(Article = main_parts_material$article_no, material_list5))


  colnames(material_list6) = gsub(" ", "_", colnames(material_list6) , fixed =
                                    TRUE)
  colnames(col_list6) = gsub(" ", "_", colnames(col_list6) , fixed = TRUE)

  # Merge the Color with the Material data
  range_data_attrib <-
    merge(
      x = col_list6,
      y = material_list6,
      by.x = "Article",
      by.y = "Article",
      all.x = TRUE
    )

  # Change all factor variables to character
  isF <- sapply(range_data_attrib, is.factor)
  range_data_attrib[isF] <-
    lapply(range_data_attrib[isF], as.character)
  sapply(range_data_attrib, class)

  return(range_data_attrib)
}
