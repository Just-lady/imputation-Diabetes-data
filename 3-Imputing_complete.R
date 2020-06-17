#copy completed_records and replace all missing values to 0s 
z<-completed_records
z[is.na(z)] <- 0

#Generating missing values patterns in the completed_records and exploring patterns
library(mice)
mypatterns<- rbind(c(1,1,1,1,1,1,1,1,1,1,1,0,0,1,1), c(1,1,1,1,0,1,1,1,1,1,1,0,1,1,1),
                    c(1,1,1,1,0,1,1,1,1,1,1,1,0,1,1), c(1,1,1,1,1,0,1,1,1,1,1,0,1,1,1), 
                    c(1,1,1,1,0,0,1,1,1,1,1,1,1,1,1), c(1,1,1,1,1,1,1,1,1,1,0,0,1,1,1),
                    c(1,1,1,1,1,1,1,1,1,1,0,1,0,1,1), c(1,1,1,1,0,1,1,1,1,1,0,1,1,1,1),
                    
                    c(1,1,1,1,0,1,1,1,1,1,1,0,0,1,1), c(1,1,1,1,0,0,1,1,1,1,1,0,1,1,1),
                    c(1,1,1,1,0,0,1,1,1,1,1,1,0,1,1), c(1,1,1,1,0,1,1,1,1,1,0,0,1,1,1),
                    c(1,1,1,1,0,1,1,1,1,1,0,1,0,1,1), c(1,1,1,1,1,0,1,1,1,1,0,0,1,1,1),
                    c(1,1,1,1,0,0,1,1,1,1,0,1,1,1,1), 
                    
                    c(1,1,1,1,0,0,1,1,1,1,1,0,0,1,1), c(1,1,1,1,0,0,1,1,1,1,0,1,0,1,1),
                    c(1,1,1,1,0,0,1,1,1,1,0,0,1,1,1),
                    
                    c(1,1,1,1,1,1,1,1,1,1,0,1,1,1,1), c(1,1,1,1,1,1,1,1,1,1,1,0,1,1,1),
                    c(1,1,1,1,0,1,1,1,1,1,1,1,1,1,1), c(1,1,1,1,1,1,1,1,1,1,1,1,0,1,1))


myfreq <- c(0.18, 0.0075, 0.0075, 0.0, 0.03, 0.2, 0.03, 0.0,
            0.009, 0.067, 0.0165, 0.0, 0.0, 0.0, 0.009,
            0.14, 0.0175, 0.06,
            0.016, 0.17, 0.009, 0.02)

result <- ampute(z[,-c(1,2,5)], prop= 0.868, freq= myfreq, patterns = mypatterns)
md.pattern(result$amp[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)


mypatterns_ <- rbind(c(1,1,1,1,0,1,1,1,1,1,0,1,1,1,1),c(1,1,1,1,1,0,1,1,1,1,1,0,1,1,1), 
                      c(1,1,1,1,1,0,1,1,1,1,0,0,1,1,1),c(1,1,1,1,0,1,1,1,1,1,0,0,1,1,1), c(1,1,1,1,0,1,1,1,1,1,0,1,0,1,1))

sam<-result$amp[complete.cases(result$amp[,c(5,6,11,12,13)]),]

res <- ampute(sam, prop= 0.9, patterns = mypatterns_)

result$amp[4, ] <- res$amp[1,]
result$amp[11, ] <- res$amp[2,]
result$amp[23, ] <- res$amp[4,]
result$amp[122, ] <- res$amp[15,]
result$amp[34, ] <- res$amp[6,]

md.pattern(result$amp[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)

#Complete result two datasets with full variables
amp<-copy_full_dataset(completed_records,result$amp)
full_copy_amp<-copy_full_dataset(completed_records,result$data)

library(mice)
md.pattern(amp[rowSums(is.na(amp[,c(5,6,11,12,13)]))==1,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)



###     Urea      ###
#
#Grouping 1-missing value Urea records
Urea_1_MV<-amp[rowSums(is.na(amp[,c(5,6,11,12,13)]))==1,]
Urea_1_MV<-subset(Urea_1_MV,is.na(Urea_1_MV$Urea))

#Imputing Urea 1-MV
library(rpart)
Urea_Model <- rpart(Urea ~ BMI+K+ Creatinine+ALT+ Glucose_Fasting +Cholesterol 
                    + HDL+LDL+AGE_AT_TEST_TIME+HbA1c,
                    data=amp[!is.na(amp$Urea), ],
                    method="anova", na.action=na.omit) 
plot(Urea_Model, margin= 0.1, uniform=TRUE, main="Regression Tree for Urea")
text(Urea_Model, use.n=TRUE, all=TRUE, cex=.8)

Urea_Pred <- predict(Urea_Model, Urea_1_MV)

#Calculate Imputation Error
Urea_C <- z[,8]
rows<-as.numeric(row.names(Urea_1_MV))
actuals <- full_copy_amp$Urea[rows]

Urea_err<- imp_err(Urea_Pred,actuals,Urea_C)

library(DMwR)
regr.eval(actuals, Urea_Pred)

#Plotting orginal and imputed values
plot(actuals, type='o', col="blue", pch="o", lty=1,
     xlab="number of records", ylab="Urea  values")
points(Urea_Pred, col="red")
lines(Urea_Pred, col="red")
legend(2,30,legend=c("actual ","imputed "), col=c("blue","red"), pch=c("o","o"),lty=1) 

#Table of values
head(full_copy_amp[rows,c(5,6)])

Urea_1_MV[,5]<-Urea_Pred
head(Urea_1_MV[,c(5,6)])

#adding imputed values in amp dataset
amp[rows,"Urea"]<-Urea_Pred


library(mice)
md.pattern(amp[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)
md.pattern(amp[rowSums(is.na(amp[,c(5,6,11,12,13)]))==1,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)


###     HbA1c      ###
#
#Grouping 1-missing value HbA1c records
H_1_MV<-amp[rowSums(is.na(amp[,c(5,6,11,12,13)]))==1,]
H_1_MV<-subset(H_1_MV,is.na(H_1_MV$HbA1c))

#Imputing HbA1c 1-MV
library(rpart)
H_Model <- rpart(HbA1c ~ BMI+K+ Urea+ Creatinine+ALT+ Glucose_Fasting +Cholesterol 
                 + HDL+LDL+AGE_AT_TEST_TIME,
                 data=amp[!is.na(amp$HbA1c), ],
                 method="anova", na.action=na.omit) 
plot(H_Model, margin= 0.1, uniform=TRUE, main="Regression Tree for HbA1c")
text(H_Model, use.n=TRUE, all=TRUE, cex=.8)
plotcp(H_Model)

H_Pred <- predict(H_Model, H_1_MV)

#Calculate Imputation Error
H_C <- z[,14]
rows<-as.numeric(row.names(H_1_MV))
actuals <- full_copy_amp$HbA1c[rows]

H_err<- imp_err(H_Pred,actuals,H_C)

regr.eval(actuals, H_Pred)

#Plotting orginal and imputed values
plot(actuals, type='o', col="blue", pch="o", lty=1,
     xlab="number of records", ylab="HbA1c  values")
points(H_Pred, col="red")
lines(H_Pred, col="red")
legend(0.5,12.8,legend=c("actual ","imputed "), col=c("blue","red"), pch=c("o","o"),lty=1) 

#Table of values
head(full_copy_amp[rows,c(11,12)])

H_1_MV[,11]<-H_Pred
head(H_1_MV[,c(11,12)])

#adding imputed values in amp dataset
amp[rows,"HbA1c"]<-H_Pred

library(mice)
md.pattern(amp[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)
md.pattern(amp[rowSums(is.na(amp[,c(5,6,11,12,13)]))==1,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)



###     Cholesterol      ###
#
#Grouping 1-missing value Cholesterol records
Chol_1_MV<-amp[rowSums(is.na(amp[,c(5,6,11,12,13)]))==1,]
Chol_1_MV<-subset(Chol_1_MV,is.na(Chol_1_MV$Cholesterol))

#Imputing Cholesterol 1-MV
library(rpart)
Chol_Model <- rpart(Cholesterol ~ BMI+K+ Creatinine+ALT+ Glucose_Fasting +Urea 
                    + HDL+LDL+AGE_AT_TEST_TIME+HbA1c,
                    data=amp[!is.na(amp$Cholesterol), ],
                    method="anova", na.action=na.omit) 
plot(Chol_Model, margin= 0.1, uniform=TRUE, main="Regression Tree for Cholesterol")
text(Chol_Model, use.n=TRUE, all=TRUE, cex=.8)

Chol_Pred <- predict(Chol_Model, Chol_1_MV)

#Calculate Imputation Error
Chol_C <- z[,16]
rows<-as.numeric(row.names(Chol_1_MV))
actuals <- full_copy_amp$Cholesterol[rows]

Chol_err<- imp_err(Chol_Pred,actuals,Chol_C)

regr.eval(actuals, Chol_Pred)

#Plotting orginal and imputed values
plot(actuals, type='o', col="blue", pch="o", lty=1,
     xlab="number of records", ylab="Cholesterol  values")
points(Chol_Pred, col="red")
lines(Chol_Pred, col="red")
legend(1,7.5,legend=c("actual ","imputed "), col=c("blue","red"), pch=c("o","o"),lty=1) 

#Table of values
head(full_copy_amp[rows,c(13,15)])

Chol_1_MV[,13]<-Chol_Pred
head(Chol_1_MV[,c(13,15)])

#adding imputed values in amp dataset
amp[rows,"Cholesterol"]<-Chol_Pred

library(mice)
md.pattern(amp[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)
md.pattern(amp[rowSums(is.na(amp[,c(5,6,11,12,13)]))==1,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)


###     Glucose_Fasting      ###
#
#Grouping 1-missing value Glucose_Fasting records
Glu_1_MV<-amp[rowSums(is.na(amp[,c(5,6,11,12,13)]))==1,]

#Imputing Glucose_Fasting 1-MV
library(rpart)
Glu_Model <- rpart(Glucose_Fasting ~ BMI+K+ Urea+ Creatinine+ALT+ HbA1c +Cholesterol 
                 + HDL+LDL+AGE_AT_TEST_TIME,
                 data=amp[!is.na(amp$Glucose_Fasting), ],
                 method="anova", na.action=na.omit) 
plot(Glu_Model, margin= 0.1, uniform=TRUE, main="Regression Tree for Glucose-Fasting")
text(Glu_Model, use.n=TRUE, all=TRUE, cex=.8)

Glu_Pred <- predict(Glu_Model, Glu_1_MV)

#Calculate Imputation Error
Glu_C <- z[,15]
rows<-as.numeric(row.names(Glu_1_MV))
actuals <- full_copy_amp$Glucose_Fasting[rows]

Glu_err<- imp_err(Glu_Pred,actuals,Glu_C)

regr.eval(actuals, Glu_Pred)

#Plotting orginal and imputed values
plot(actuals, type='o', col="blue", pch="o", lty=1,
     xlab="number of records", ylab="Glucose-Fasting  values")
points(Glu_Pred, col="red")
lines(Glu_Pred, col="red")
legend(0.2,45,legend=c("actual ","imputed "), col=c("blue","red"), pch=c("o","o"),lty=1) 

#Table of values
head(full_copy_amp[rows,c(11,12)])

Glu_1_MV[,12]<-Glu_Pred
head(Glu_1_MV[,c(11,12)])

#adding imputed values in amp dataset
amp[rows,"Glucose_Fasting"]<-Glu_Pred

library(mice)
md.pattern(amp[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)


###      Multi Missing Values       ###
#

#Extracting complete records for training clustering model
amp_Complete <-amp[complete.cases(amp[,c(5,6,11,12,13)]), ]

#K-mean Clustering 
#K-mean Clustering based on The 5 most important variables:

#Standarzing completed_records values
m<-scale(amp_Complete[,-c(16:19)])

kmdata<- as.matrix(m[,c(5,6,11,12,13)])

#determining the optimal number of clusters for k-means clustering:
library(factoextra) 
fviz_nbclust(kmdata,kmeans,method = "wss") +
             geom_vline(xintercept = 4, linetype = 2)+
             labs(subtitle = "Elbow method")

# Calculates clustering indices
library(NbClust)
NbClust(amp_Complete[,-c(16:18)], distance = "euclidean", min.nc = 2, max.nc = 8,
        method = "complete", index = "all", alphaBeale = 0.1)

km = kmeans(kmdata,4, nstart=25)
km

amp_Complete$cluster <- as.factor(km$cluster)

#Grouping each cluster in completed_clusters
complete_clusters<-classify(amp_Complete)

#Determine the value of K for KNN classification
#Scale training dataset
library(caret)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(cluster ~ Urea+Cholesterol+HbA1c+Glucose_Fasting+Creatinine, 
                 data = amp_Complete, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
plot(knn_fit)


###      HbA1c Multi Missing Values       ###
#
#Extracting all HbA1c missing value records
H_MV <-amp[is.na(amp[,11]), ]

#KNN calssification in the H_MV Dataset:
#extract training set
train_data<- m[,c(5,6,11,12,13)]
##extract testing set
test_data<- scale(H_MV[,c(5,6,11,12,13)])
test_data[is.na(test_data)] <- 0

##extract (cluster) column of train dataset because it will be used as 'cl' argument in knn function.
train_cluster <- amp_Complete[,19]

#knn calssification:
##Run knn calssification:
library(class)
Knn_Class <- as.vector(knn(train_data,test_data,train_cluster,k=7))

plot(Knn_Class)

H_MV$cluster<-Knn_Class

#Grouping each cluster in H_MV
H_clusters<-classify(H_MV)

aggregate(amp_Complete[,-c(16:19)], by = list(cluster=km$cluster), FUN = mean, na.rm=TRUE)

#Imputing HbA1c with CMC method:
#Missing values pattern
H_MV_imp<-impute(H_clusters,complete_clusters,"HbA1c", "Glucose_Fasting")

#Evaluation
#Calculate Imputation Error
H_Pred<-unlist(lapply(H_MV_imp, function(y) y[,11]))
actuals <- full_copy_amp$HbA1c[is.na(amp$HbA1c)]

H_err<- imp_err(H_Pred,actuals,H_C)

regr.eval(actuals, H_Pred)

#Table of values
head(full_copy_amp[is.na(amp$HbA1c),c(11,12)])

#adding imputed values in amp dataset
rows<-as.numeric(unlist(lapply(H_MV_imp, row.names)))
amp$HbA1c[rows]<- H_Pred

library(mice)
md.pattern(amp[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)



###     Creatinine      ###
#
##   No 1-MV pattern with Creatinine     ##

###     Creatinine Multi missing values      ###
#
#Extracting all Creatinine missing value records
Cr_MV <-amp[is.na(amp[,6]), ]

#KNN calssification in the Cr_MV Dataset:
#extract training set
train_data  #the previouse train dataset
##extract testing set
test_data<- scale(Cr_MV[,c(5,6,11,12,13)])
test_data[is.na(test_data)] <- 0

##extract (cluster) column of train dataset because it will be used as 'cl' argument in knn function.
train_cluster #the previouse train cluster

#knn calssification:
##Run knn calssification:
library(class)
Knn_Class <- as.vector(knn(train_data,test_data,train_cluster,k=7))

plot(Knn_Class)

Cr_MV$cluster<-Knn_Class

#Grouping each cluster in Cr_MV
Cr_clusters<-classify(Cr_MV)

aggregate(amp_Complete[,-c(16:19)], by = list(cluster=km$cluster), FUN = mean, na.rm=TRUE)

#Imputing Creatinine with CMC method:
#Missing values pattern
Cr_MV_imp<-impute(Cr_clusters,complete_clusters,"Creatinine", "Urea")

#Evaluation
#Calculate Imputation Error
Cr_C<-z[,9]
Cr_Pred<-unlist(lapply(Cr_MV_imp, function(y) y[,6]))
actuals <- full_copy_amp$Creatinine[is.na(amp$Creatinine)]

Cr_err<- imp_err(Cr_Pred,actuals,Cr_C)

regr.eval(actuals, Cr_Pred)


#Table of values
head(full_copy_amp[is.na(amp$Creatinine),c(5,6)])

#adding imputed values in amp dataset
rows<-as.numeric(unlist(lapply(Cr_MV_imp, row.names)))
amp$Creatinine[rows]<- Cr_Pred

library(mice)
md.pattern(amp[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)


###     Urea      ###
#
#Grouping 1-missing value Urea records
#
##      starting from line 60 to 98    ##

library(mice)
md.pattern(amp[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)


###     clustering complete records       ###
#
##      starting from line 251 to 291    ##


###     Urea Multi missing values      ###
#
#Extracting all Urea missing value records
Urea_MV <-amp[is.na(amp[,5]), ]

#KNN calssification in the Urea_MV Dataset:
#extract training set
train_data<- m[,c(5,6,11,12,13)]
##extract testing set
test_data<- scale(Urea_MV[,c(5,6,11,12,13)])
test_data[is.na(test_data)] <- 0

##extract (cluster) column of train dataset because it will be used as 'cl' argument in knn function.
train_cluster <- amp_Complete[,19]


#knn calssification:
##Run knn calssification:
library(class)
Knn_Class <- as.vector(knn(train_data,test_data,train_cluster,k=5))

plot(Knn_Class)
Urea_MV$cluster<-Knn_Class

#Grouping each cluster in Urea_MV
Urea_clusters<-classify(Urea_MV)

aggregate(amp_Complete[,-c(16:19)], by = list(cluster=km$cluster), FUN = mean, na.rm=TRUE)

#Imputing Urea with CMC method:
#Missing values pattern
Urea_MV_imp<-impute(Urea_clusters,complete_clusters,"Urea", "Creatinine")

#Evaluation
#Calculate Imputation Error
Urea_Pred<-unlist(lapply(Urea_MV_imp, function(y) y[,5]))
actuals <- full_copy_amp$Urea[is.na(amp$Urea)]

Urea_err<- imp_err(Urea_Pred,actuals,Urea_C)

regr.eval(actuals, Urea_Pred)

#adding imputed values in amp dataset
rows<-as.numeric(unlist(lapply(Urea_MV_imp, row.names)))
amp$Urea[rows]<- Urea_Pred

library(mice)
md.pattern(amp[,c(5,6,11,12,13)])



###     Cholesterol      ###
#
#Grouping 1-missing value Urea records
#
##   starting from line 157 to 195    ##

amp[amp == 0] <- NA

library(mice)
md.pattern(amp[,c(5,6,11,12,13,15)])

amp_Complete[is.na(amp_Complete)]<- 0

###     clustering complete records       ###
#
##      starting from line 251 to 290    ##


###     Cholesterol Multi missing values      ###
#
#Extracting all Cholesterol missing value records
Chol_MV <-amp[is.na(amp[,13]), ]

#KNN calssification in the Chol_MV Dataset:
#extract training set
train_data<- m[,c(5,6,11,12,13)]
##extract testing set
test_data<- scale(Chol_MV[,c(5,6,11,12,13)])
test_data[is.na(test_data)] <- 0

##extract (cluster) column of train dataset because it will be used as 'cl' argument in knn function.
train_cluster <- amp_Complete[,19]

#knn calssification:
##Run knn calssification:
library(class)
Knn_Class <- as.vector(knn(train_data,test_data,train_cluster,k=15))

plot(Knn_Class)
Chol_MV$cluster<-Knn_Class

#Grouping each cluster in Chol_MV
Chol_clusters<-classify(Chol_MV)

aggregate(amp_Complete[,-c(16:19)], by = list(cluster=km$cluster), FUN = mean, na.rm=TRUE)

#Imputing Cholesterol with CMC method:
#Missing values pattern
Chol_MV_imp<-impute(Chol_clusters,complete_clusters,"Cholesterol", "LDL")

#Evaluation
#Calculate Imputation Error
Chol_Pred<-unlist(lapply(Chol_MV_imp, function(y) y[,13]))
actuals <- full_copy_amp$Cholesterol[is.na(amp$Cholesterol)]

Chol_err<- imp_err(Chol_Pred,actuals,Chol_C)

regr.eval(actuals, Chol_Pred)

#adding imputed values in amp dataset
rows<-as.numeric(unlist(lapply(Chol_MV_imp, row.names)))
amp$Cholesterol[rows]<- Chol_Pred

library(mice)
md.pattern(amp[,c(5,6,11,12,13,15)])
md.pattern(amp[,c(5,6,11,12,13)])

###     Glucose_Fasting      ###
#
##   starting from line 205 to 241    ##


library(mice)
md.pattern(amp[,c(5,6,11,12,13)])

amp_Prposed_11<-amp
amp<-copy_full_dataset(completed_records,result$amp)




library(imputeTS)
amp_Complete <- na_replace(amp_Complete, 0)
