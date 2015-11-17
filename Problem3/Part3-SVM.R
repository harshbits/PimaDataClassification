library("e1071")

#Problem 3 - SVM Classifier

#Load the pima data.
#You can directly pass and get output as well.
#PimaData<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data",header=FALSE,sep=",")

#Here I will pass the data as a command line argument.

#Ex:- in Windows  Rscript Part3-SVM.R https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data
args <- commandArgs(TRUE)
PimaData<-read.table(file=args[1],header=FALSE,sep=",")


#Part 1
#for Default kernal
avgdefault <- 0
for(j in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  model <- svm(V9~., data = training)
  predsvm <- predict(model,testing, type="raw")
  l<-1
  count<-0
  
  for(i in 1:length(predsvm))
  {
    if(unname(predsvm[i])>0.5)
    {
      count[l]=1
      l<-l+1
    }
    else
    {
      count[l]=0
      l<-l+1
    }
    
  }
  accu[j]<-(sum(testing$V9==count)/length(count))*100
  avgdefault <- avgdefault + accu[j]
}
avgdefault <- avgdefault/10
#This is accuracy using Default Kernel.
avgdefault

#######################################################

#Part 2 - Using different Kernel values.
#for kernal=linear
accu<-0
avglinear <- 0
for(j in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  model <- svm(V9~., kernel="linear", data = training)
  predsvm <- predict(model,testing, type="raw")
  l<-1
  count<-0
  
  for(i in 1:length(predsvm))
  {
    if(unname(predsvm[i])>0.5)
    {
      count[l]=1
      l<-l+1
    }
    else
    {
      count[l]=0
      l<-l+1
    }
    
  }
  accu[j]<-(sum(testing$V9==count)/length(count))*100
  avglinear <- avglinear + accu[j]
}
avglinear <- avglinear/10

#for kernal=polynomial
avgpoly <- 0
for(j in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  model <- svm(V9~., kernel="polynomial", data = training)
  predsvm <- predict(model,testing, type="raw")
  l<-1
  count<-0
  
  for(i in 1:length(predsvm))
  {
    if(unname(predsvm[i])>0.5)
    {
      count[l]=1
      l<-l+1
    }
    else
    {
      count[l]=0
      l<-l+1
    }
    
  }
  accu[j]<-(sum(testing$V9==count)/length(count))*100
  avgpoly <- avgpoly + accu[j]
}
avgpoly <- avgpoly/10



#for kernal=radial
avgradial <- 0
for(j in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  model <- svm(V9~., kernel="radial", data = training)
  predsvm <- predict(model,testing, type="raw")
  l<-1
  count<-0
  
  for(i in 1:length(predsvm))
  {
    if(unname(predsvm[i])>0.5)
    {
      count[l]=1
      l<-l+1
    }
    else
    {
      count[l]=0
      l<-l+1
    }
    
  }
  accu[j]<-(sum(testing$V9==count)/length(count))*100
  avgradial <- avgradial + accu[j]
}
avgradial <- avgradial/10

#for kernel=sigmoid

avgsig <- 0
#This will generate loop for 10 times.
for(j in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  model <- svm(V9~., kernel="sigmoid", data = training)
  predsvm <- predict(model,testing, type="raw")
  l<-1
  count<-0
  
  for(i in 1:length(predsvm))
  {
    if(unname(predsvm[i])>0.5)
    {
      count[l]=1
      l<-l+1
    }
    else
    {
      count[l]=0
      l<-l+1
    }
    
  }
  accu[j]<-(sum(testing$V9==count)/length(count))*100
  avgsig <- avgsig + accu[j]
}
#Average of 10 values.
avgsig <- avgsig/10

accu <- c(avglinear,avgpoly,avgradial,avgsig)
ker <- c("Linear","Poynomial","Radial","Sigmoid")
svmout <- data.frame(Kernel=ker, Accuracy=accu)


#For Default Data,
cat("Using Default Kernel :")
avgdefault
#Print the PimaData in tabular format
svmout