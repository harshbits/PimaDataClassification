library("class")


#Problem 4: k-Nearest	Neighbor	classifier.	

#Load the pima data.
#You can directly pass and get output as well.
#PimaData<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data",header=FALSE,sep=",")

#Here I will pass the data as a command line argument.

#Ex:- in Windows  Rscript Part4-KNN.R https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data
args <- commandArgs(TRUE)
PimaData<-read.table(file=args[1],header=FALSE,sep=",")

accu<-0
#Using k = 3
knn3 <- 0
for(i in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  #Classification Factor
  cl <- as.factor(training[['V9']])
  knnModel<-knn(train=training, test=testing, cl, k=3, prob=TRUE)
  accu[i]<-mean(knnModel == testing[ , "V9"])*100 
  knn3 <- knn3 + accu[i]
}
knn3 <- knn3/10

#Using k = 5
knn5 <-0
for(i in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  #Classification Factor
  cl <- as.factor(training[['V9']])
  knnModel<-knn(train=training, test=testing, cl, k=5, prob=TRUE)
  accu[i]<-mean(knnModel == testing[ , "V9"])*100 
  knn5 <- knn5+ accu[i]
}
knn5 <- knn5/10

#Using k = 7
knn7 <-0
for(i in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  #Classification Factor
  cl <- as.factor(training[['V9']])
  knnModel<-knn(train=training, test=testing, cl, k=7, prob=TRUE)
  accu[i]<-mean(knnModel == testing[ , "V9"])*100 
  knn7 <- knn7 + accu[i]
  
}
knn7 <- knn7/10

#Using k = 9
knn9 <- 0
for(i in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  #Classification Factor
  cl <- as.factor(training[['V9']])
  knnModel<-knn(train=training, test=testing, cl, k=9, prob=TRUE)
  accu[i]<-mean(knnModel == testing[ , "V9"])*100
  knn9 <- knn9 + accu[i]
}
knn9 <- knn9/10

#Using k = 11
knn11<-0
for(i in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  #Classification Factor
  cl <- as.factor(training[['V9']])
  knnModel<-knn(train=training, test=testing, cl, k=11, prob=TRUE)
  accu[i]<-mean(knnModel == testing[ , "V9"])*100 
  knn11 <- knn11 + accu[i]
}
knn11 <- knn11/10


accu <- c(knn3,knn5,knn7,knn9,knn11)
k <- c("3",5,7,9,11)
knnOut <- data.frame(K=k, Accuracy=accu)
#Output will print here in the form of Tabular Data.
knnOut
