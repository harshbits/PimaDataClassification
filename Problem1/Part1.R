library("e1071")


#You can directly pass and get output as well.
#PimaData<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data",header=FALSE,sep=",")

#Here I will pass the data as a command line argument.

#Ex:- in Windows  Rscript Part1.R https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data
args <- commandArgs(TRUE)
PimaData<-read.table(file=args[1],header=FALSE,sep=",")

#Problem 1- Exploratory PimaData Analysis

#Part 1
#Nuber of times Pregnant
#First Histogram
#Second Barplot
hist(PimaData$V1,main="Histogram for N_P",xlab="No_of_times_pregnant",col="Yellow",las=1,breaks=10)
barplot(xtabs(~PimaData$V1),xlab="No_of_pregnancies",main="Barplot of No_of_pregnancies",ylim=c(0,170))

#P-G Concentration
hist(PimaData$V2,main="Histogram for P-G concentration",xlab="P-G concentration",col="Yellow",las=1,breaks=10)
barplot(xtabs(~PimaData$V2),xlab="P-G concentration",main="Barplot of P-G concentration",ylim=c(0,25))

#Diastolic B.P.
hist(PimaData$V3,main="Histogram for Diastolic B.P.",xlab="Diastolic B.P.",col="Yellow",las=1,breaks=10)
barplot(xtabs(~PimaData$V3),xlab="Diastolic B.P.",main="Barplot of Diastolic B.P.",ylim=c(0,70))

#T. Skin Foldthickness
hist(PimaData$V4,main="Histogram for T. skin fold thickness",xlab="T. skin fold thickness",col="Yellow",las=1,breaks=10)
barplot(xtabs(~PimaData$V4),xlab="T. skin fold thickness",main="Barplot of T. skin fold thickness",ylim=c(0,150))

#Serum Insulin
hist(PimaData$V5,main="Histogram for serum insulin",xlab="serum insulin",col="Yellow",las=1,breaks=10)
barplot(xtabs(~PimaData$V5),xlab="serum insulin",main="Barplot of serum insulin",ylim=c(0,25))

#BMI
hist(PimaData$V6,main="Histogram for BMI",xlab="BMI",col="Yellow",las=1,breaks=10)
barplot(xtabs(~PimaData$V6),xlab="BMI",main="Barplot of BMI",ylim=c(0,20))

#Diabetes pedigree
hist(PimaData$V7,main="Histogram for Diabetes pedigree ",xlab="Diabetes pedigree",col="Yellow",las=1,breaks=10)
barplot(xtabs(~PimaData$V7),xlab="Diabetes pedigree",main="Barplot of Diabetes pedigree",ylim=c(0,8))

#Age
hist(PimaData$V8,main="Histogram for Age ",xlab="Age ",col="Yellow",las=1,breaks=10)
barplot(xtabs(~PimaData$V8),xlab="Age ",main="Barplot of Age ",ylim=c(0,100))

#Class -> it has 0 and 1 value 
#This is Class Variable.
hist(PimaData$V9,main="Histogram for Class",xlab="Class",col="Yellow",las=1,breaks=10)
barplot(xtabs(~PimaData$V9),xlab="Class",main="Barplot of Class",ylim=c(0,550))

#Part 2 - Find Correlation between each of the attributes and class.
mvalue<-0
name<-0
length <- ncol(PimaData)-1
for(i in 1:length){
  temp <-(cor(PimaData[,names(PimaData[i])],PimaData[,"V9"])) 
  if(i == 1){
	mvalue<-temp
	}
	#It will check maximum value, and return maximum value.
  if(temp > mvalue){
    mvalue<-temp 
    name<-names(PimaData[i])
	}
}
sprintf("strong correlation with class is= %s and value=%s",name,mvalue)

#Part 3 - Compute the correlation	between	all	pairs	of	the	8	attributes.
count<-0
#2 loops for comparing all attributes with everyone of the other attributes.
for(i in 1:length)
{
	k<-i+1
	if(k<=length){
		for(j in k:length){
			temp<-(cor(PimaData[,names(PimaData[i])],PimaData[,names(PimaData[j])]))
			if(count==0){
				mvalue<-temp
				count<-1    
			}
			if(temp>mvalue){
			mvalue<-temp
			name<-c(names(PimaData[i]),names(PimaData[j]))
			}
		}
	}
}
cat("Strong correlation with Attribute is=", name," and its value=", mvalue)