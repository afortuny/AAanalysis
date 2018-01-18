

# load required functions

source("R/analytical_settings.R")
source("R/assortment_analysis.R")
source("R/attribute_guidance.R")
source("R/computation_inc_sales.R")
source("R/transferable_demand.R")
source("R/price_fabric_splits.R")
source("R/fabrics_transf.R")
source("R/assortment_analysis.R")
source("R/technical_settings.R")

area <- 'Apparel'

# Report code progress
report_progress <- report_progress_factory(area)

#get analytical _settings

analytical_settings<-get_analytical_settings()


#read data frame
APP_AB_data <- readr::read_csv("data/input_APP/APP_AB_data.csv")
APP_AB_data<-APP_AB_data[!duplicated(APP_AB_data),]
views <- read.csv("data/input_APP/APP_AB_views.csv")
views<-as.data.frame(views)
views<-views[!duplicated(views),]


colnames(views)[1:2]<-c("Article_Number","Grouping")
#assign data frame to object
data<-APP_AB_data
colnames(data)[1:5]<-c( "Article_Number","Grouping","norm_sales","ARP","fabric")
data<-as.data.frame(data)


#eliminate strange levels
for (c in 6:ncol(data)){
  data[,c]<-gsub(" ","_", data[,c] , fixed=TRUE)
  data[,c]<-gsub(",","_", data[,c] , fixed=TRUE)
  data[,c]<-gsub("]","", data[,c] , fixed=TRUE)
  data[,c]<-gsub("[","", data[,c] , fixed=TRUE)
  data[,c]<-gsub(" ","_", data[,c] , fixed=TRUE)
  data[,c]<-gsub("(","", data[,c] , fixed=TRUE)
  data[,c]<-gsub(")","", data[,c] , fixed=TRUE)
  data[,c]<-gsub("/","_", data[,c] , fixed=TRUE)

}

#eliminate column names

for (c in 6:ncol(data)){
  colnames(data)[c]<-gsub(" ","_", colnames(data)[c] , fixed=TRUE)
  colnames(data)[c]<-gsub(",","_", colnames(data)[c] , fixed=TRUE)
  colnames(data)[c]<-gsub("]","", colnames(data)[c] , fixed=TRUE)
  colnames(data)[c]<-gsub("[","", colnames(data)[c] , fixed=TRUE)
  colnames(data)[c]<-gsub(" ","_", colnames(data)[c] , fixed=TRUE)
  colnames(data)[c]<-gsub("(","", colnames(data)[c] , fixed=TRUE)
  colnames(data)[c]<-gsub(")","", colnames(data)[c] , fixed=TRUE)
  colnames(data)[c]<-gsub("/","_", colnames(data)[c] , fixed=TRUE)

}


#make each column with right format

data[,"norm_sales"]<-as.numeric(data[,"norm_sales"])
data[,"ARP"]<-as.numeric(data[,"ARP"])
data[,"fabric"]<-as.factor(data[,"fabric"])


#required packages
library(sqldf)
library(glmnet)
library(dummies)

#run
output<-assortment_analysis(data)

#merge the views

#for the top5
top5<-as.data.frame(output[1])
top5<-top5[-1,]
views_delete<-views
colnames(views_delete)<-paste0("article_to_delete","-",colnames(views_delete))
top5_views<-merge(top5,views_delete,by.x=c("Article_id_to_delete","Grouping"),by.y=c("article_to_delete-Article_Number","article_to_delete-Grouping"),all.x=TRUE)
views_keep<-views
colnames(views_keep)<-paste0("article_to_keep","-",colnames(views_keep))
top5_views<-merge(top5_views,views_keep,by.x=c("Article_id_to_keep","Grouping"),by.y=c("article_to_keep-Article_Number","article_to_keep-Grouping"),all.x=TRUE)

write.csv(top5_views,"top5_views.csv")


#for the attribute guidance

attribute_guidance_report<-as.data.frame(output[2])
attribute_guidance_report<-attribute_guidance_report[-1,]

attribute_guidance_report_views<-merge(attribute_guidance_report,views,
                                       by.x=c("Article_ID","Grouping"),by.y=c("Article_Number","Grouping"),all.x=TRUE)


##
write.csv(attribute_guidance_report_views,"attribute_guidance_report_views.csv")


## area
area<-"Footwear"



