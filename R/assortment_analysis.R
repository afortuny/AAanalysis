#'
#' Assortment Analysis
#'
#' It is the main function that runs all of the assortment analysis using all the functions made for that purpose. The code is a wraper of functions
#' that provide guidance on which are the relevant attribute to explain sales performance, and the article drop recommendation.
#'
#'
#' requires sqldf package
#'
#' @param  data The required data to run the analysis : Article,Groupings,Volume,Price,Attributes
#'
#' @author Alan Fortuny <Alan.FortunySicart@@adidas-group.com>
#' @return A list containing two data frames, the attribute guidance report and the top 5
#'
#------------------------------------------------------------------------

assortment_analysis <-
  function(data){


    ###########################################################################################################################################
    ### 1. Load data

    #data <- Data
    #data<-as.data.frame(Benefit_analysis_data_FTW)
    #data<-data[-which(data["fabric"]=="500<"),]
    data<-subset(data,data[,"norm_sales"]>=1)
    data<-subset(data,data[,"ARP"]>=1)
    ### 2. Data creation


    #top5 report empty matrix
    top5<-matrix(ncol=11)
    colnames(top5)<-c("article_to_delete","article_to_keep","demand_transfer","demand_transfer_perc","sales_to_delete", "grouping","ranking",
                      "new_total_sales_keep","total_sales_transfer_relative","total_sales_lost_relative","sales_lost_absolut")


    #attribute guidance empty report
    attribute_guidance_report<-matrix(ncol=20)
    colnames(attribute_guidance_report)<-c("Article_Number", "secondary_attribute_name","primary_attribute_name",
                                           "secondary_attribute_value", "primary_attribute_perc","rank",
                                           "primary_attribute_name_rank_1","primary_attribute_perc_rank_1","secondary_attribute_name_rank_1",
                                           "secondary_attribute_value_rank_1","primary_attribute_name_rank_2","primary_attribute_perc_rank_2" ,
                                           "secondary_attribute_name_rank_2","secondary_attribute_value_rank_2","primary_attribute_name_rank_3" ,
                                           "primary_attribute_perc_rank_3","secondary_attribute_name_rank_3","secondary_attribute_value_rank_3",
                                           "Grouping","Total_coefficient_attribute_top3_bundle")




    ### 3.Run analysis

    for (l in as.character(unique(data[,analytical_settings$Grouping]))) {



      #select the combination for which you want to run the analysis

      data_group <-subset(data,data[,analytical_settings$Grouping]==l)

      #report progress bar
      report_progress_group <-
        report_progress$subarea(key = 'Group',l)


      #chech that there is enough data

      if(nrow(data_group)>=analytical_settings$sample_size_thresh){






        # 3.1 Definition of fabric and price groupings
        #decide if splitting by price and fabric should be performed and thus allow demand transferability only within the specified splits
        #It creates the grouped fabric variable (fabric_group) and prints information regarding fabric, fabric_group and price distribution

        data_group <-
          price_fabric_splits(
            data_group =data_group,
            report_progress = report_progress_group)


        ### For apparel: By default, the analysis will split by fabric and price segment, and include fabric for the regression and fabric
        ###              group for the splits
        ### For footwear: By default, the analysis will split by price group, since fabric is not included


        ###3.2 definition of attributes to analyze . Note that fabric will be in the regression, but not fabric group since it is a split

        if (any(colnames(data_group)==analytical_settings$fabric)){

          columns_filter<-c(analytical_settings$Article_id,
                            analytical_settings$Volume,
                            "fabric_group",
                            "price_cat",
                            analytical_settings$Grouping)
        } else {
          columns_filter<-c(analytical_settings$Article_id,
                            analytical_settings$Volume,
                            "price_cat",
                            analytical_settings$Grouping)

        }


        ### those are the primary attributes to use

        primary_attributes<-colnames(data_group)[-which(colnames(data_group) %in% columns_filter)]

        #Calculate attribute importance and support attribute guidance


        attribute_guidance_list<-attribute_guidance(data_group=data_group ,
                                                    primary_attributes= primary_attributes,
                                                    group=l)


        attribute_importance_guidance_table<-as.data.frame(attribute_guidance_list[1])



        attribute_guidance_report<-rbind(attribute_guidance_report, attribute_importance_guidance_table)

        #primary attribute importance weights and names

        prim_attr_df<-as.data.frame(attribute_guidance_list[2])



        if(all(!is.na(attribute_importance_guidance_table))){




          ## Computation of Incremental Sales

          #calculate the incremental sales for each combination of price and fabric weight cluster

          #create groups

          if(any(colnames(data_group)==analytical_settings$fabric)){

            data_group[,"splits"]<-paste0(data_group[,c("fabric_group")],"_",data_group[,c("price_cat")])
          } else {

            data_group[,"splits"]<-paste0(data_group[,c("price_cat")])

          }

          for (splits in as.character(unique(data_group[,"splits"]))){

            data_group_split<-data_group[which(data_group[,"splits"] %in% splits),]

            #report progress bar
            report_progress_split <-report_progress_group$subarea(splits)

            if(nrow(data_group_split)>5){




              ### 7. Transferable Demand
              #Transferability function run




              top5_c <-
                transferable_demand(
                  raw_range_data_att =data_group_split,
                  prim_attr_weights= as.numeric(prim_attr_df[,"primary_attribute_perc"])/100,
                  analysis_filters =l,
                  retain_threshold = analytical_settings$retain_threshold,
                  primary_attributes = as.character(prim_attr_df[,"primary_attribute_name"]),
                  grouping=l,
                  report_progress = report_progress_split)



              if(all(!is.na(top5_c))){
                top5<-rbind(top5,as.data.frame(top5_c[1]))
              }else{
                report_progress$info("No article deletion")
              }

            }else{



              report_progress_split$info("Insufficient data for that split")
            }

          }



        } else {


          report_progress_group$info("Not significant variables for that group")
        }


      } else {

        report_progress_group$info("Insufficient data for that group")

      }

    }


    ### 4.Rename columns of the reports

    colnames(top5)<-c("Article_id_to_delete","Article_id_to_keep","Absolute_Sales_Transferred",
                      "Transferability_Percentage","Sales_to_delete","Grouping",
                      "Ranking_to_delete","New_Total_Sales_receiving_sales","Total_Sales_Transferability",
                      "Sales_Lost Relative","Sales_Lost Absolute")


    attribute_guidance_report[,"Secondary_Attribute_Name_Grouped"]<-attribute_guidance_report[,"secondary_attribute_name"]

    colnames(attribute_guidance_report)<-c("Article_ID", "Secondary_Attribute_Name","Primary_Attribute_Name",
                                           "Secondary_Attribute_Value", "Primary_Attribute_Value","Rank",
                                           "Primary_Attribute_Name_RANK1","Primary Attribute Value_RANK1","Secondary_Attribute_Name_RANK1",
                                           "Secondary_Attribute_Value_RANK1","Primary_Attribute_Name_RANK2","Primary Attribute Value_RANK2","Secondary_Attribute_Name_RANK2",
                                           "Secondary_Attribute_Value_RANK2","Primary_Attribute_Name_RANK3","Primary Attribute Value_RANK3","Secondary_Attribute_Name_RANK3",
                                           "Secondary_Attribute_Value_RANK3","Grouping","Total_avg_value_of_ secondary_attributes_in bundle","Secondary_Attribute_Name_Grouped")





    return(list(top5,attribute_guidance_report))

  }



###################################################################################################################################
