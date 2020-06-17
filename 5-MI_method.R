# perform mice imputation, based on random forests.
library(mice)
library(randomForest)

amp_2 <- mice(amp[,-c(16:18)],m=5,maxit=50,meth='pmm')
summary(amp_2)

# generate the completed data.
miceOutput <- complete(amp_2) 


library(mice)
md.pattern(miceOutput[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)
md.pattern(amp[,c(5,6,11,12,13)], plot = TRUE, rotate.names = TRUE)


#compute the accuracy of Urea
orginal <- full_copy_amp$Urea[is.na(amp$Urea)]
predicteds <- miceOutput[is.na(amp$Urea), "Urea"]

Urea_err<- imp_err(predicteds,orginal,Urea_C)

library(DMwR)
regr.eval(orginal, predicteds)


#compute the accuracy of HbA1c
orginal <- full_copy_amp$HbA1c[is.na(amp$HbA1c)]
predicteds <- miceOutput[is.na(amp$HbA1c), "HbA1c"]

H_err<- imp_err(predicteds,orginal,H_C)

regr.eval(orginal, predicteds)


#compute the accuracy of Creatinine
orginal <- full_copy_amp$Creatinine[is.na(amp$Creatinine)]
predicteds <- miceOutput[is.na(amp$Creatinine), "Creatinine"]

Cr_err<- imp_err(predicteds,orginal,Cr_C)

regr.eval(orginal, predicteds)


#compute the accuracy of Cholesterol
orginal <- full_copy_amp$Cholesterol[is.na(amp$Cholesterol)]
predicteds <- miceOutput[is.na(amp$Cholesterol), "Cholesterol"]

Chol_err<- imp_err(predicteds,orginal,Chol_C)

regr.eval(orginal, predicteds)


#compute the accuracy of Glucose_Fasting
orginal <- full_copy_amp$Glucose_Fasting[is.na(amp$Glucose_Fasting)]
predicteds <- miceOutput[is.na(amp$Glucose_Fasting), "Glucose_Fasting"]

Glu_err<- imp_err(predicteds,orginal,Glu_C)

regr.eval(orginal, predicteds)

#Plotting orginal and imputed values
# 5 figures arranged in 2 rows and 3 columns
plot(full_copy_amp$HbA1c[is.na(amp$HbA1c)], type='o', col="green", pch="o", lty=1,
     xlab="number of records", ylab="HbA1c  values")
points(miceOutput$HbA1c[is.na(amp$HbA1c)], col="red")
lines(miceOutput$HbA1c[is.na(amp$HbA1c)], col="red")
points(amp_Prposed$HbA1c[is.na(amp$HbA1c)], col="blue")
lines(amp_Prposed$HbA1c[is.na(amp$HbA1c)], col="blue")
legend(0,14,legend=c("actual ","imputed MI", "Proposed imputation"), col=c("green","red", "blue"), pch=c("o","o"),lty=1) 


plot(full_copy_amp$Urea[is.na(amp$Urea)], type='o', col="green", pch="o", lty=1,
     xlab="number of records", ylab="Urea  values")
points(miceOutput$Urea[is.na(amp$Urea)], col="red")
lines(miceOutput$Urea[is.na(amp$Urea)], col="red")
points(amp_Prposed$Urea[is.na(amp$Urea)], col="blue")
lines(amp_Prposed$Urea[is.na(amp$Urea)], col="blue")
legend(0,60,legend=c("actual ","imputed MI", "Proposed imputation"), col=c("green","red", "blue"), pch=c("o","o"),lty=1) 


plot(full_copy_amp$Creatinine[is.na(amp$Creatinine)], type='o', col="green", pch="o", lty=1,
     xlab="number of records", ylab="Creatinine  values")
points(miceOutput$Creatinine[is.na(amp$Creatinine)], col="red")
lines(miceOutput$Creatinine[is.na(amp$Creatinine)], col="red")
points(amp_Prposed$Creatinine[is.na(amp$Creatinine)], col="blue")
lines(amp_Prposed$Creatinine[is.na(amp$Creatinine)], col="blue")
legend(0,800,legend=c("actual ","imputed MI", "Proposed imputation"), col=c("green","red", "blue"), pch=c("o","o"),lty=1) 



plot(full_copy_amp$Cholesterol[is.na(amp$Cholesterol)], type='o', col="green", pch="o", lty=1,
     xlab="number of records", ylab="Cholesterol  values")
points(miceOutput$Cholesterol[is.na(amp$Cholesterol)], col="red")
lines(miceOutput$Cholesterol[is.na(amp$Cholesterol)], col="red")
points(amp_Prposed$Cholesterol[is.na(amp$Cholesterol)], col="blue")
lines(amp_Prposed$Cholesterol[is.na(amp$Cholesterol)], col="blue")
legend(0,9,legend=c("actual ","imputed MI", "Proposed imputation"), col=c("green","red", "blue"), pch=c("o","o"),lty=1) 



plot(full_copy_amp$Glucose_Fasting[is.na(amp$Glucose_Fasting)], type='o', col="green", pch="o", lty=1,
     xlab="number of records", ylab="Glucose_Fasting  values")
points(miceOutput$Glucose_Fasting[is.na(amp$Glucose_Fasting)], col="red")
lines(miceOutput$Glucose_Fasting[is.na(amp$Glucose_Fasting)], col="red")
points(amp_Prposed$Glucose_Fasting[is.na(amp$Glucose_Fasting)], col="blue")
lines(amp_Prposed$Glucose_Fasting[is.na(amp$Glucose_Fasting)], col="blue")
legend(0,30,legend=c("actual ","imputed MI", "Proposed imputation"), col=c("green","red", "blue"), pch=c("o","o"),lty=1) 




#Statistical Analysis
# show some descriptive statistics of the attributes
library(skimr)

summary(completed_records[,c(8,9,14,15,16)])
skim(completed_records[,c(8,9,14,15,16)])

summary(miceOutput[,c(5,6,11,12,13)])
skim(miceOutput[,c(5,6,11,12,13)])


summary(amp_Prposed[,c(5,6,11,12,13)])
skim(amp_Prposed[,c(5,6,11,12,13)])
