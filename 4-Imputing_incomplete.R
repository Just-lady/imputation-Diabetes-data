#copy incompleted_records and replace all missing values to 0s 
z<-incompleted_records

library(mice)
md.pattern(z[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)
md.pattern(z[rowSums(is.na(z[,c(8,9,14,15,16)]))==1,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)


###     Urea      ###
#
#Grouping 1-missing value Urea records
Urea_1_MV<-z[rowSums(is.na(z[,c(8,9,14,15,16)]))==1,]
Urea_1_MV<-subset(Urea_1_MV,is.na(Urea_1_MV$Urea))

#Imputing Urea_1-MV
library(rpart)
Urea_Model <- rpart(Urea ~ BMI+K+ Creatinine+ALT+ Glucose_Fasting +Cholesterol 
                    + HDL+LDL+AGE_AT_TEST_TIME+HbA1c,
                    data=Diabetes_Dataset[!is.na(Diabetes_Dataset$Urea), ],
                    method="anova", na.action=na.omit) 

plot(Urea_Model, margin= 0.1, uniform=TRUE, main="Regression Tree for Urea")
text(Urea_Model, use.n=TRUE, all=TRUE, cex=.8)
plotcp(Urea_Model)

Urea_Pred <- predict(Urea_Model, Urea_1_MV)

#adding imputed values in z dataset
index<-1
for (record in 1:nrow(z)){
  if((rowSums(is.na(z[record,c(8,9,14,15,16)]))==1)
     & (is.na(z[record,"Urea"]))){
    z$Urea[record]<- Urea_Pred[index]
    index <- index +1
  }
}

md.pattern(z[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)
md.pattern(z[rowSums(is.na(z[,c(8,9,14,15,16)]))==1,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)


###     HbA1c      ###
#
#Grouping 1-missing value HbA1c records
H_1_MV<-z[rowSums(is.na(z[,c(8,9,14,15,16)]))==1,]
H_1_MV<-subset(H_1_MV,is.na(H_1_MV$HbA1c))

#Imputing HbA1c 1-MV
H_Model <- rpart(HbA1c ~ BMI+K+ Urea+ Creatinine+ALT+ Glucose_Fasting +Cholesterol 
                 + HDL+LDL+AGE_AT_TEST_TIME,
                 data=Diabetes_Dataset[!is.na(Diabetes_Dataset$HbA1c), ],
                 method="anova", na.action=na.omit) 
plot(H_Model, margin= 0.1, uniform=TRUE, main="Regression Tree for HbA1c")
text(H_Model, use.n=TRUE, all=TRUE, cex=.8)
plotcp(H_Model)

H_Pred <- predict(H_Model, H_1_MV)


#adding imputed values in amp dataset
index<-1
for (record in 1:nrow(z)){
  if((rowSums(is.na(z[record,c(8,9,14,15,16)]))==1)
     & (is.na(z[record,"HbA1c"]))){
    z$HbA1c[record]<- H_Pred[index]
    index <- index +1
  }
}

md.pattern(z[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)
md.pattern(z[rowSums(is.na(z[,c(8,9,14,15,16)]))==1,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)



###     Cholesterol      ###
#
#Grouping 1-missing value Cholesterol records
Chol_1_MV<-z[rowSums(is.na(z[,c(8,9,14,15,16)]))==1,]
Chol_1_MV<-subset(Chol_1_MV,is.na(Chol_1_MV$Cholesterol))

#Imputing Cholesterol 1-MV
Chol_Model <- rpart(Cholesterol ~ BMI+K+ Creatinine+ALT+ Glucose_Fasting +Urea 
                    + HDL+LDL+AGE_AT_TEST_TIME+HbA1c,
                    data=Diabetes_Dataset[!is.na(Diabetes_Dataset$Cholesterol), ],
                    method="anova", na.action=na.omit) 
plot(Chol_Model, margin= 0.1, uniform=TRUE, main="Regression Tree for Cholesterol")
text(Chol_Model, use.n=TRUE, all=TRUE, cex=.8)
plotcp(Chol_Model)

Chol_Pred <- predict(Chol_Model, Chol_1_MV)

#adding imputed values in amp dataset
index<-1
for (record in 1:nrow(z)){
  if((rowSums(is.na(z[record,c(8,9,14,15,16)]))==1)
     & (is.na(z[record,"Cholesterol"]))){
    z$Cholesterol[record]<- Chol_Pred[index]
    index <- index +1
  }
}

md.pattern(z[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)
md.pattern(z[rowSums(is.na(z[,c(8,9,14,15,16)]))==1,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)



###     Glucose_Fasting      ###
#
#Grouping 1-missing value Glucose_Fasting records
Glu_1_MV<-z[rowSums(is.na(z[,c(8,9,14,15,16)]))==1,]

#Imputing Glucose_Fasting 1-MV
Glu_Model <- rpart(Glucose_Fasting ~ BMI+K+ Urea+ Creatinine+ALT+ HbA1c +Cholesterol 
                   + HDL+LDL+AGE_AT_TEST_TIME,
                   data=Diabetes_Dataset[!is.na(Diabetes_Dataset$Glucose_Fasting), ],
                   method="anova", na.action=na.omit) 
plot(Glu_Model, margin= 0.1, uniform=TRUE, main="Regression Tree for Glucose-Fasting")
text(Glu_Model, use.n=TRUE, all=TRUE, cex=.8)
plotcp(Glu_Model)

Glu_Pred <- predict(Glu_Model, Glu_1_MV)


#adding imputed values in amp dataset
index<-1
for (record in 1:nrow(z)){
  if((rowSums(is.na(z[record,c(8,9,14,15,16)]))==1)
     & (is.na(z[record,"Glucose_Fasting"]))){
    z$Glucose_Fasting[record]<- Glu_Pred[index]
    index <- index +1
  }
}

md.pattern(z[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)
md.pattern(z[rowSums(is.na(z[,c(8,9,14,15,16)]))==1,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)



###      Multi Missing Values       ###
#

#Extracting complete records for training clustering model
z_Complete <-z[complete.cases(z[,c(8,9,14,15,16)]), ]
z_Complete <- rbind(z_Complete, Diabetes_Dataset[complete.cases(Diabetes_Dataset[,c(8,9,14,15,16)]), ])

#K-mean Clustering 
#K-mean Clustering based on The 5 most important variables:

#Standarzing completed_records values
m<-as.data.frame(scale(z_Complete[,-c(1,2,5)]))

kmdata<- as.matrix(m[,c(5,6,11,12,13)])

#Determining the optimal number of clusters for k-means clustering:
library(factoextra) 
fviz_nbclust(kmdata,kmeans,method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Calculates clustering indices
library(NbClust)
NbClust(z_Complete[,-c(1,2,5)], distance = "euclidean", min.nc = 2, max.nc = 8,
        method = "complete", index = "all", alphaBeale = 0.1)


km = kmeans(kmdata,3, nstart=25)
km

z_Complete$cluster <- as.factor(km$cluster)

aggregate(z_Complete[,-c(1,2,5,19)], by = list(cluster=km$cluster), FUN = mean, na.rm=TRUE)

#Vlaues of attributes in each clusters
library(GGally)
library(plotly)

p <- ggparcoord(data = z_Complete, columns = c(8,9,14,15,16), groupColumn = "cluster", scale = "std") + labs(x = "Patients constituent", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p)

#Grouping each cluster in completed clusters
clusters<-classify(z_Complete)

#Determine the value of K for KNN classification
#Scale training dataset
library(caret)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(cluster ~ Urea+Cholesterol+HbA1c+Glucose_Fasting+Creatinine, 
                 data = z_Complete, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
plot(knn_fit)



###      HbA1c Multi Missing Values       ###
#
#Extracting all HbA1c missing value records
H_MV_D <-z[is.na(z[,14]), ]

#KNN calssification in the H_MV_D Dataset:
#extract training set
train_data<- z_Complete[,c(8,9,14,15,16)]
##extract testing set
test_data<- H_MV_D[,c(8,9,14,15,16)]
test_data[is.na(test_data)] <- 0

##extract (cluster) column of train dataset because it will be used as 'cl' argument in knn function.
train_cluster <- z_Complete$cluster

#knn calssification:
##Run knn calssification:
library(class)
Knn_Class <- as.vector(knn(train_data,test_data,train_cluster,k=19))

plot(Knn_Class)

H_MV_D$cluster<-Knn_Class

#Grouping each cluster in H_MV
H_clusters<-classify(H_MV_D)

#Imputing HbA1c with CMC method:
#Missing values pattern
H_MV_imp<-impute_2(H_clusters,clusters,"HbA1c", "Glucose_Fasting")

#adding imputed values in amp dataset
H_<-rbind(H_MV_imp[[1]], H_MV_imp[[2]],H_MV_imp[[3]],H_MV_imp[[4]])

z<- z[complete.cases(z[,14]), ]
z<-rbind(z,H_[,-19])


md.pattern(z[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)



###     Creatinine      ###
#
##   No 1-MV pattern with Creatinine     ##


###     Creatinine Multi missing values      ###
#
#Extracting all Creatinine missing value records
Cr_MV_D <-z[is.na(z[,9]), ]

#KNN calssification in the Cr_MV Dataset:
#extract training set
train_data  #the previouse train dataset
##extract testing set
test_data<- Cr_MV_D[,c(8,9,14,15,16)]
test_data[is.na(test_data)] <- 0

##extract (cluster) column of train dataset because it will be used as 'cl' argument in knn function.
train_cluster #the previouse train cluster

#knn calssification:
##Run knn calssification:
library(class)
Knn_Class <- as.vector(knn(train_data,test_data,train_cluster,k=5))

plot(Knn_Class)

Cr_MV_D$cluster<-Knn_Class

#Grouping each cluster in Cr_MV
Cr_clusters<-classify(Cr_MV_D)

#Imputing Creatinine with CMC method:
#Missing values pattern
Cr_MV_imp<-impute_2(Cr_clusters,clusters,"Creatinine", "Urea")

#adding imputed values in amp dataset
Cr_<-rbind(Cr_MV_imp[[1]],Cr_MV_imp[[3]])

z<- z[complete.cases(z[,9]), ]
z<-rbind(z,Cr_[,-19])


md.pattern(z[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)



###     Urea      ###
#
##   starting from line 10 to 36s    ##



###     clustering complete records       ###
#
##      starting from line 140 to 194    ##



###      Urea Multi Missing Values       ###
#
#Extracting all Urea missing value records
Urea_MV_D <-z[is.na(z[,8]), ]

#KNN calssification in the H_MV_D Dataset:
#extract training set
train_data<- z_Complete[,c(8,9,14,15,16)]
##extract testing set
test_data<- Urea_MV_D[,c(8,9,14,15,16)]
test_data[is.na(test_data)] <- 0

##extract (cluster) column of train dataset because it will be used as 'cl' argument in knn function.
train_cluster <- z_Complete$cluster

#knn calssification:
##Run knn calssification:
library(class)
Knn_Class <- as.vector(knn(train_data,test_data,train_cluster,k=9))

plot(Knn_Class)

Urea_MV_D$cluster<-Knn_Class

#Grouping each cluster in H_MV
Urea_clusters<-classify(Urea_MV_D)

#Imputing Urea with CMC method:
#Missing values pattern
Urea_MV_imp<-impute_2(Urea_clusters,clusters,"Urea", "Creatinine")

#adding imputed values in amp dataset
Urea_<-rbind(Urea_MV_imp[[2]],Urea_MV_imp[[1]],Urea_MV_imp[[4]])

z<- z[complete.cases(z[,8]), ]
z<-rbind(z,Urea_[,-19])


md.pattern(z[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)



###     Cholesterol      ###
#
##   starting from line 78 to 100    ##


###     clustering complete records       ###
#
##      starting from line 140 to 194    ##



###      Cholesterol Multi Missing Values       ###
#
#Extracting all Cholesterol missing value records
Chol_MV_D <-z[is.na(z[,16]), ]

#KNN calssification in the H_MV_D Dataset:
#extract training set
train_data<- z_Complete[,c(8,9,14,15,16)]
##extract testing set
test_data<- Chol_MV_D[,c(8,9,14,15,16)]
test_data[is.na(test_data)] <- 0

##extract (cluster) column of train dataset because it will be used as 'cl' argument in knn function.
train_cluster <- z_Complete$cluster

#knn calssification:
##Run knn calssification:
library(class)
Knn_Class <- as.vector(knn(train_data,test_data,train_cluster,k=13))

plot(Knn_Class)

Chol_MV_D$cluster<-Knn_Class

#Grouping each cluster in Chol_MV_D
Chol_clusters<-classify(Chol_MV_D)

#Imputing Cholesterol with CMC method:
#Missing values pattern
Chol_MV_imp<-impute_2(Chol_clusters,clusters,"Cholesterol", "LDL")

#adding imputed values in amp dataset
Chol_<-rbind(Chol_MV_imp[[1]],Chol_MV_imp[[2]],Chol_MV_imp[[3]])

z<- z[complete.cases(z[,16]), ]
z<-rbind(z,Chol_[,-19])


md.pattern(z[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)



###     Glucose_Fasting      ###
#
##   starting from line 110 to  133    ##

Imp_Diabetes_Dataset<-rbind(z,completed_records)

#exploring missing values in Diabetes Dataset
library(naniar)
vis_miss(Imp_Diabetes_Dataset, sort_miss = TRUE)


# show some descriptive statistics of the attributes
summary(Imp_Diabetes_Dataset)
library(skimr)
skim(Imp_Diabetes_Dataset[,c(8,9,14,15,16)])


library(mice)
md.pattern(Imp_Diabetes_Dataset[,c(8,9,14,15,16)], plot = TRUE, rotate.names = TRUE)


#exploring missing values in Diabetes Dataset
library(naniar)
vis_miss(Diabetes_Dataset, sort_miss = TRUE)
library(ggplot2)
gg_miss_var(Diabetes_Dataset, show_pct=TRUE) + ylim(0, 100)
