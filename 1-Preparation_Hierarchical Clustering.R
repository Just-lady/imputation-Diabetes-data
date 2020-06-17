# import an excel file of the type 2 diabetes patients data and determine the columns type
library(readxl)
Diabetes_Dataset <- read_excel("C:/Users/96656/Dropbox/Master_Thesis/Files/data/Final_Dataset/Diabetes_Dataset.xlsx", 
                               col_types = c("skip", "text", "text", "numeric",
                                             "numeric", "numeric", "numeric", 
                                             "date", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric"), na = "NA")

#Delete the height and weight:
Diabetes_Dataset<-Diabetes_Dataset[,-c(4,5)]

# examine the imported dataset and its variables type
str(Diabetes_Dataset)

# Convert Gender to 2-level factor
Diabetes_Dataset$Gender<- as.factor(Diabetes_Dataset$Gender)

# number of male and female patients
library(dplyr)
Patients<- distinct(data.frame(Diabetes_Dataset$PATID, Diabetes_Dataset$Gender))
summary(Patients)

# show some descriptive statistics of the attributes
summary(Diabetes_Dataset)
library(skimr)
skim(Diabetes_Dataset)

#Delete the Triglycerides attribute
Diabetes_Dataset<-Diabetes_Dataset[,-c(19)]
summary(Diabetes_Dataset)

# draw the scatter plot matrix for the most important variables
pairs(Diabetes_Dataset[,-c(1:7,10:13,17,18)], main = " The Most Important variables of Diabetes Patients")

#Standarding Diabetes_Dataset values
z<-scale(Diabetes_Dataset[,-c(1,2,5)])
summary(z)
# Hierarchical Clustering with Bootstrapped p-values
library(pvclust)
main_Hcl <- pvclust(z)

# dendogram with p-values
plot(main_Hcl,cex = 0.6, hang = -1) 
# add rectangles around groups highly supported by the data
pvrect(main_Hcl, alpha=0.95)

#exploring missing values in Diabetes Dataset
library(naniar)
vis_miss(Diabetes_Dataset, sort_miss = TRUE)
vis_miss(Diabetes_Dataset[,c(8,9,14,15,16)], sort_miss = TRUE)
library(ggplot2)
gg_miss_var(Diabetes_Dataset, show_pct=TRUE) + ylim(0, 100)

library(VIM)
aggr(Diabetes_Dataset, numbers = TRUE, prop =FALSE)
aggr(Diabetes_Dataset, combined = TRUE, numbers = TRUE)
?aggr()

#Grouping diabetes dataset records to completed and uncompleted
##according to the 5 most important variables 
completed_records <- Diabetes_Dataset[complete.cases(Diabetes_Dataset[,c(8,9,14,15,16)]), ]
incompleted_records <- Diabetes_Dataset[!complete.cases(Diabetes_Dataset[,c(8,9,14,15,16)]), ]

# inspecting missing values patterns in Diabetes dataset
library(mice)
md.pattern(incompleted_records[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)

mean(is.na(Diabetes_Dataset))


library(VIM)
spineMiss(Diabetes_Dataset)
matrixplot(Diabetes_Dataset)
barMiss(incompleted_records[,c(8,9,14,15,16)])
