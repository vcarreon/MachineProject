#Packages Needed
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)
library(rattle)

#Start by downloading the training and test sets
if(!file.exists("data")){
  dir.create("data")
}
fileURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileURL,destfile="./data/training.csv")
training <- read.csv(url(fileURL), na.strings=c("NA","#DIV/0!",""))


fileURL2<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileURL2,destfile="./data/testing.csv")
testing <- read.csv(url(fileURL2), na.strings=c("NA","#DIV/0!",""))

#look at the classe variable- it is a factor with 5 levels
str(training$classe)
#What are the dimensions
dim(trainClasse)

#Data Cleaning
#first find all columns with names for the relevant variables
trainingaccel<-grepl("^accel",names(training))
trainingtotal<-grepl("^total",names(training))
roll<-grepl("^roll",names(training))
pitch<-grepl("^pitch",names(training))
yaw<-grepl("^yaw",names(training))
magnet<-grepl("^magnet",names(training))
gyro<-grepl("^gyro",names(training))
#Create dataframes with subsets of the data for the relevant variables
acceldata<-training[ ,trainingaccel]
totaldata<-training[,trainingtotal]
rolldata<-training[ ,roll]
pitchdata<-training[ ,pitch]
yawdata<-training[,yaw]
magnetdata<-training[,magnet]
gyrodata<-training[,gyro]
#Combine all these data frames together and include column 160, which is the "classe" we will use as a predictor.
trainClasse<-cbind(acceldata,rolldata,pitchdata,yawdata,magnetdata,gyrodata,totaldata,training[ ,160])
#Add "Classe" as a name to last column 
colnames(trainClasse)[53]<-'Classe'
str(trainClasse)
dim(trainClasse) #Now we have 19622 observations and 53 variables

#Do same data cleanup to the testing set
dim(testing) #20 observations, and 160 variables
#first find all columns with names for the relevant variables
testingaccel<-grepl("^accel",names(testing))
testingtotal<-grepl("^total",names(testing))
troll<-grepl("^roll",names(testing))
tpitch<-grepl("^pitch",names(testing))
tyaw<-grepl("^yaw",names(testing))
tmagnet<-grepl("^magnet",names(testing))
tgyro<-grepl("^gyro",names(testing))
#Create dataframes with subsets of the data for the relevant variables
tacceldata<-testing[ ,testingaccel]
ttotaldata<-testing[,testingtotal]
trolldata<-testing[ ,troll]
tpitchdata<-testing[,tpitch]
tyawdata<-testing[,tyaw]
tmagnetdata<-testing[,tmagnet]
tgyrodata<-testing[,tgyro]
#Combine all these data frames together and include column 160, which is the "problem ID" we will use as a predictor.
testClasse<-cbind(tacceldata,trolldata,tpitchdata,tyawdata,tmagnetdata,tgyrodata,ttotaldata,testing[ ,160])
#Add "Problem ID" as a name to last column 
colnames(testClasse)[53]<-'problem.id'
dim(testClasse) #now we have 20 observations and 53 variables

#set up my own testing and training from original training set
set.seed(400)
inTrain <- createDataPartition(training$classe, p=0.6, list=FALSE)
myTraining <- trainClasse[inTrain, ]
myTesting <- trainClasse[-inTrain, ]
dim(myTraining); dim(myTesting)

#Model 1: Decision tree with rpart model
set.seed(400)
modFit<-train(Classe~.,method="rpart",data=myTraining)
print(modFit$finalModel)
fancyRpartPlot(modFit$finalModel,cex=.5, under.cex=1,shadow.offset=0,main="Decision Tree for Classe")

#Predict with the rpart model
classepredict=predict(modFit,myTesting)
confusionMatrix(myTesting$Classe,classepredict)
#The accuracy of this model is only .5459. This model is most accurate for Class E but is least accurate for Class D.

#Model 2: Next try Random Forrest with cross validation with 4 folds
set.seed(400)
modFit2<-train(Classe~.,method="rf",trControl=trainControl(method="cv",number=4),data=myTraining)
print(modFit2$finalModel)
print(modFit2)
#which variables are important?
varImp(modFit2)
plot(modFit2)

#predict with the Random Forest Model
classepredict2=predict(modFit2,myTesting)
confusionMatrix(myTesting$Classe,classepredict2)

#How does the accuracy look?
#Accuracy is .992. If it so high, could we be overfitting?

#Model 3: Boosting with gbm method
set.seed(400)
modFit3<-train(Classe~.,method="gbm",trControl=trainControl(method="repeatedcv",number=5,repeats=1),data=myTraining,verbose=F)
print(modFit3)
varImp(modFit3)
plot(modFit3,ylim=c(.9,1))

#predict with the Boosting model
classepredict3=predict(modFit3,myTesting)
confusionMatrix(myTesting$Classe,classepredict3)

#How does accuracy look?
#Accuracy is .9605

#Now predict on test data with the best model, which is Random Forests
predictTest<-predict(modFit2,testing)
predictTest
