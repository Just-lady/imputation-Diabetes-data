#copy datasets
copy_full_dataset<- function(a, b){
  full_amp <- b
  full_amp$PATID <- a[,1]
  full_amp$Gender <- a[,2]
  full_amp$Date_Taken <- a[,5]
  
  full_amp
}


#Spliting missing-values records in a variable of the rest records in a dataset:
split_missing <-function(a, column1){
  
  missing_records <-a[FALSE,]
  
  missing_records<-a[rowSums(is.na(a[,c(5,6,11,12,13)]))==1,]
  missing_records<-subset(missing_records,is.na(missing_records$column1))
  
    missing_records
  }



#Imputation Error
imp_err <- function(x,y,a){
  sum((abs(y-x))/(max(a)-min(a)))/length(x)
}


#classifying records according to classes:
classify_2<- function(a){
  
  cluster_1<-a[FALSE,]
  cluster_2 <-a[FALSE,]
  cluster_3 <-a[FALSE,]
  cluster_4 <-a[FALSE,]
  
  for (record in 1:nrow(a)) {
    if(a[record,"cluster"]==1){
      newRecord <- data.frame(a[record,])
      cluster_1 <- rbind(cluster_1, newRecord)
    }else if(a[record,"cluster"]==2){
      newRecord <- data.frame(a[record,])
      cluster_2 <- rbind(cluster_2, newRecord)}
   else if(a[record,"cluster"]==3){
    newRecord <- data.frame(a[record,])
    cluster_3 <- rbind(cluster_3, newRecord)}
    else {
      newRecord <- data.frame(a[record,])
      cluster_4 <- rbind(cluster_4, newRecord)} 
  }
  clusters<- list(cluster_1, cluster_2, cluster_3, cluster_4) 
}


#imputing 2-missing-value patterns
impute<- function(a,b, column1, column2){
  for (i in seq_len(length(a))) {
    for (record in seq_len(nrow(a[[i]]))) {
      if(!is.na(a[[i]][record,column2])){
        d<-which.min(abs(b[[i]][,column2]-(a[[i]][record,column2])))
        a[[i]][record,column1]<- b[[i]][d,column1]
      }else{
        a[[i]][record,column1]<- mean(b[[i]][,column1])
      } 
    }
  }
  a
}




classify<- function(a){
  
  cluster_1<- subset(a, cluster==1)
  cluster_2 <-subset(a, cluster==2)
  cluster_3 <-subset(a, cluster==3)
  cluster_4 <-subset(a, cluster==4)
  
  clusters<- list(cluster_1, cluster_2, cluster_3, cluster_4) 
}
