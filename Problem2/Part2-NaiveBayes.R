library("e1071")

# Part 2- Naive Bayes Classification

#Load the pima data.
#You can directly pass and get output as well.
#PimaData<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data",header=FALSE,sep=",")

#Here I will pass the data as a command line argument.

#Ex:- in Windows  Rscript Part2-NaiveBayes.R https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data
args <- commandArgs(TRUE)
PimaData<-read.table(file=args[1],header=FALSE,sep=",")

#accu prediction for 10 experiments
accu<-0
for(j in 1:10){
  train <- sample(1:nrow(PimaData), size=(0.9*nrow(PimaData)))
  training <- PimaData[train, ]
  testing <- PimaData[-train, ]
  
  #generating naiveBayes model for given training data.
  model <- naiveBayes(V9 ~ ., data = training)
  prediction<-predict(model, testing, type = "raw")
  count<-0
  l<-1
  for(i in 1:nrow(prediction))
  {
    if(prediction[i]>0.5)
    {
      count[l]=0
      l<-l+1
      
    }
    else
    {
      count[l]=1
      l<-l+1
    }
  }
  
  accu[j]<-(sum(testing$V9==count)/length(count))*100
  
}
#printing values of all experiments
n <- c("1",2,3,4,5,6,7,8,9,10)
output <- data.frame(Experiment=n, Accuracy=accu)

#This will print the tablular form output.
output