
#Data Mining Project



#From the general structure of the dataset, it is very clean, with no missing data and clear structure.
#quantitative attributes: Season in which the analysis was performed, Age at the time of analysis, Childish diseases, 
#Accident or serious trauma, Surgical intervention, High fevers, Frequency of alcohol consumption, Smoking habit, 
#qualitative attributes : Diagnosis(N,O)
#Categorizing attribute "diagnosis" of fertility_data dataset into categories N or O.


#setting working directory
fertility_data=read.table("D:/MS_COURSE/SecondSem/Datamining/project/tosubmit/final/fertility_Diagnosis_Data_set.txt",sep =",", header = FALSE, dec =".")
fix(fertility_data)

#coorelation matrix
pairs(fertility_data)

#dimensions of fertility_data dataset
dim(fertility_data)

#summary characteristics of fertility_data dataset
summary(fertility_data)

#check the general structure of the dataset

str(fertility_data)

#-------------------------------------------------------------------------------------------------------------------

fertility_data=read.table("D:/MS_COURSE/SecondSem/Datamining/project/tosubmit/final/fertility_Diagnosis_Data_set.txt",sep =",", header = FALSE, dec =".")
fix(fertility_data)

library(randomForest)

High=ifelse(fertility_data$V10 == "N","Yes","No")
fertility_data=data.frame(fertility_data,High)
fertility_data=fertility_data[,-10]


fix(fertility_data)
dim(fertility_data)


SampleData_fertility = sample(1:nrow(fertility_data),67)
TrainSet_fertility = fertility_data[SampleData_fertility,]
TestSet_fertility = fertility_data[-SampleData_fertility,]
fertility.test = fertility_data[-SampleData_fertility,]
fertility.train = fertility_data[SampleData_fertility,]
High.test=High[-SampleData_fertility]



fix(TrainSet_fertility)
dim(TrainSet_fertility)

fix(TestSet_fertility)
dim(TestSet_fertility)

fix(fertility_data)

#-------------------------------------------------------------------------------------------------------------------
#-Decision tree - holdout method

library(tree)

set.seed(123)
tree.fertility=tree(High~.,fertility.train)
tree.pred=predict(tree.fertility,fertility.test,type="class")
table(tree.pred,High.test)
mean(tree.pred!=High.test)


#-------------------------------------------------------------------------------------------------------------------
#Decision tree - bagging 
# number of sample column is equal to mtry (m = p)

set.seed(123)
tree.fertility=randomForest(High~.,fertility.train, ntree=500,mtry=9)
tree.pred=predict(tree.fertility,fertility.test,type="class")
table(tree.pred,High.test)
mean(tree.pred!=High.test)



#-------------------------------------------------------------------------------------------------------------------
#-Decision tree - random forest

# with mtry=2

set.seed(123)
tree.fertility=randomForest(High~.,fertility.train, ntree=500,mtry=2)
tree.pred=predict(tree.fertility,fertility.test,type="class")
table(tree.pred,High.test)
mean(tree.pred!=High.test) 	


# with mtry=4

set.seed(123)
tree.fertility=randomForest(High~.,fertility.train, ntree=500,mtry=4)
tree.pred=predict(tree.fertility,fertility.test,type="class")
table(tree.pred,High.test)
mean(tree.pred !=High.test)



#------------------------------------------------------------------------------------------------------------------------------------------
##Decission Tree Boosting method - Implementation for fertility_data dataset


#Splitting dataset - fertility_data into Train and Test set
#Here we are considering 1/3rd of the dataset as Test Set and 2/3rd of dataset as Train Set 

#setting working directory
fertility_data=read.table("D:/MS_COURSE/SecondSem/Datamining/project/tosubmit/final/fertility_Diagnosis_Data_set.txt",sep =",", header = FALSE, dec =".")
fix(fertility_data)

RNGkind(sample.kind = "Rounding")

library(gbm)

set.seed(123)
High=ifelse(fertility_data$V10 == "N","Yes","No")
fertility_data=data.frame(fertility_data,High)
fertility_data=fertility_data[,-10]


fertility_data$High=ifelse(fertility_data$High=="Yes",1,0)



set.seed(123)
train=sample(1:nrow(fertility_data),67)
fertility_data.train=fertility_data[train,]
fertility_data.test=fertility_data[-train,]
High.test=High[-train]

SampleData_fertility = sample(1:nrow(fertility_data),67)

fertility.test = fertility_data[-SampleData_fertility,]
fertility.train = fertility_data[SampleData_fertility,]



tree.fertility= gbm(High~.,fertility_data.train,distribution = "bernoulli",n.trees = 500)
tree.pred.prob=predict(tree.fertility,fertility_data.test, n.trees=500, type="response")
tree.pred=ifelse(tree.pred.prob>0.5, "Yes", "No")
table(High.test, tree.pred)

#display the error rate using boosting technique
mean(tree.pred!=High.test)

 
#-------------------------------------------------------------------------------------------------------------------
##Naïve Bayes Method 

#setting working directory
fertility_data=read.table("D:/MS_COURSE/SecondSem/Datamining/project/tosubmit/final/fertility_Diagnosis_Data_set.txt",sep =",", header = FALSE, dec =".")
fix(fertility_data)



library(e1071)

attach(fertility_data)

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(V10~., fertility_data)
Naive_Bayes_Model

NB_Predictions=predict(Naive_Bayes_Model,fertility_data)


table(NB_Predictions,V10)
mean(NB_Predictions!=V10)
mean(NB_Predictions==V10)



#Splitting dataset - fertility_data into Train and Test set
#Here we are considering 1/3rd of the dataset as Test Set and 2/3rd of dataset as Train Set 


set.seed(123)
SampleData_fertility = sample(1:nrow(fertility_data),67)
TrainSet_fertility = fertility_data[SampleData_fertility,]
TestSet_fertility = fertility_data[-SampleData_fertility,]
#test.label=V10[-TrainSet_fertility ]



fix(TrainSet_fertility)
dim(TrainSet_fertility)



fix(TestSet_fertility)
dim(TestSet_fertility)



fertility.train = fertility_data[SampleData_fertility,]
fertility.test = fertility_data[-SampleData_fertility,]



tree.naiveBayes = naiveBayes(fertility_data$V10~.,fertility_data,TrainSet_fertility)
naiveBayes.pred = predict(tree.naiveBayes,TrainSet_fertility)



table(naiveBayes.pred,fertility.train$V10)



ferility.pred.train.acc = mean(naiveBayes.pred==fertility.train$V10 )*100
ferility.pred.train.acc
#ferility.pred.train.acc is 86.56716.



ferility.pred.train.err = 100-ferility.pred.train.acc
ferility.pred.train.err
#ferility.pred.train.err is 13.43284.




naiveBayes.pred = predict(tree.naiveBayes,TestSet_fertility)
table(naiveBayes.pred,fertility.test$V10)



fertility.pred.test.acc = mean(naiveBayes.pred==fertility.test$V10)*100
fertility.pred.test.acc
#fertility.pred.test.acc is 96.9697


fertility.pred.test.err = 100-fertility.pred.test.acc
fertility.pred.test.err
#fertility.pred.test.err is 3.030303




barplot(c(ferility.pred.train.acc,fertility.pred.test.err,fertility.pred.test.acc,fertility.pred.test.err),ylim = c(0,100),main = "Naïve Bayes Method - fertility_Dataset ",

        ylab ="Percentages",col=c("blue","red"),names.arg =c("Training Accuracy","Training Error Rate","Test Accuracy","Test Error Rate"),legend=c("Accuracy","Error Rate"),args.legend = list(title="Percentages",x="topright",cex=0.8))

# Traning accuracy low than test accuracy which are indicated by blue bar, while tranning error rate and test error rate are same which indicated in red bar.

#------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------


##SVM (Linear Kernel) Method  

#Linear kernel
#Using the tune( ) function to select an optimal cost. Considering cost values 0.001, 0.01, 0.1, 1, 10, 100.

#setting working directory

fertility_data=read.table("D:/MS_COURSE/SecondSem/Datamining/project/tosubmit/final/fertility_Diagnosis_Data_set.txt",sep =",", header = FALSE, dec =".")
fix(fertility_data)


library(e1071)

set.seed(123)
tune.out = tune(svm,V10~.,data=TrainSet_fertility,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,10,100)))
summary(tune.out)
bestmodel = tune.out$best.model
bestmodel

summary(bestmodel)



svm.pred.train = predict(bestmodel,TrainSet_fertility)
table(svm.pred.train,TrainSet_fertility$V10)
fertility.pred.train.acc <- mean(svm.pred.train==TrainSet_fertility$V10)*100
fertility.pred.train.acc
#fertility.pred.train.acc is 83.58209.



fertility.pred.train.err <- 100- fertility.pred.train.acc
fertility.pred.train.err
#fertility.pred.train.err is 16.41791.



svm.pred.test = predict(bestmodel,TestSet_fertility)
table(svm.pred.test,TestSet_fertility$V10)
fertility.pred.test.acc = mean(svm.pred.test==TestSet_fertility$V10)*100
fertility.pred.test.acc
#fertility.pred.test.acc is 96.9697.



fertility.pred.test.err = 100-fertility.pred.test.acc
fertility.pred.test.err
#fertility.pred.test.err is 3.030303.



barplot(c(fertility.pred.train.acc,fertility.pred.test.err,fertility.pred.test.acc,fertility.pred.test.err),ylim = c(0,100),main = "SVM (Linear Kernel) Method - cost = 10 - fertility_Dataset",ylab ="Percentages",col=c("blue","red"),names.arg =c("Training Accuracy","Training Error Rate","Test Accuracy","Test Error Rate"),legend=c("Accuracy","Error Rate"),args.legend = list(title="Percentages",x="topright",cex=0.8))


# Traning accuracy low than test accuracy which are indicated by blue bar, while tranning error rate and test error rate are same which indicated in red bar.
#----------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------

##SVM (radial kernel) Method 

#Radial kernel
#Using the tune( ) function to select an optimal cost. Considering cost values 0.001, 0.01, 0.1, 1, 10, 100., gamma values 0.5, 1, 2, 3, 4

fertility_data=read.table("D:/MS_COURSE/SecondSem/Datamining/project/tosubmit/final/fertility_Diagnosis_Data_set.txt",sep =",", header = FALSE, dec =".")
fix(fertility_data)


library(e1071)

set.seed(123)
tune.out.svm = tune(svm,V10~.,data=TrainSet_fertility,kernel="radial",gamma=c(0.1,0.5,1,1.5,2),ranges=list(cost=c(0.01,0.1,1,10,100)))
summary(tune.out.svm)
bestmodel = tune.out.svm$best.model
bestmodel

summary(bestmodel)



svm.pred.train = predict(bestmodel,TrainSet_fertility)
table(svm.pred.train,TrainSet_fertility$V10)
fertility.pred.train.acc = mean(svm.pred.train==TrainSet_fertility$V10)*100
fertility.pred.train.acc
#fertility.pred.train.acc is 83.58209.



fertility.pred.train.err = 100-fertility.pred.train.acc
fertility.pred.train.err
#fertility.pred.train.err is 16.41791.



svm.pred.test = predict(bestmodel,TestSet_fertility)
table(svm.pred.test,TestSet_fertility$V10)
fertility.pred.test.acc = mean(svm.pred.test==TestSet_fertility$V10)*100
fertility.pred.test.acc
#fertility.pred.test.acc is 96.9697.



fertility.pred.test.err = 100-fertility.pred.test.acc
fertility.pred.test.err
#fertility.pred.test.err is 3.030303.



barplot(c(fertility.pred.train.acc,fertility.pred.test.err,fertility.pred.test.acc,fertility.pred.test.err),ylim = c(0,100),main = "SVM (Radial Kernel) Method - fertility_Dataset ",

        ylab ="Percentages",col=c("blue","red"),names.arg =c("Training Accuracy","Training Error Rate","Test Accuracy","Test Error Rate"),legend=c("Accuracy","Error Rate"),args.legend = list(title="Percentages",x="topright",cex=0.6))
		
# Traning accuracy low than test accuracy which are indicated by blue bar, while tranning error rate and test error rate are same which indicated in red bar.
#----------------------------------------------------------------------------------------------------------------------------------------

##SVM (polynomial kernel) Method 

#Radial kernel

#Using the tune( ) function to select an optimal cost. Considering cost values 0.001, 0.01, 0.1, 1, 10, 100., gamma values 0.5, 1, 2, 3, 4



fertility_data=read.table("D:/MS_COURSE/SecondSem/Datamining/project/tosubmit/final/fertility_Diagnosis_Data_set.txt",sep =",", header = FALSE, dec =".")
fix(fertility_data)

library(e1071)

set.seed(123)
tune.out.svm = tune(svm,V10~.,data=TrainSet_fertility,kernel="poly",gamma=c(0.1,0.5,1,1.5,2),ranges=list(cost=c(0.01,0.1,1,10,100)))
summary(tune.out.svm)
bestmodel = tune.out.svm$best.model
bestmodel

summary(bestmodel)



svm.pred.train = predict(bestmodel,TrainSet_fertility)
table(svm.pred.train,TrainSet_fertility$V10)
fertility.pred.train.acc = mean(svm.pred.train==TrainSet_fertility$V10)*100
fertility.pred.train.acc
#fertility.pred.train.acc is 83.58209.



fertility.pred.train.err = 100-fertility.pred.train.acc
fertility.pred.train.err
#fertility.pred.train.err is 16.41791.



svm.pred.test = predict(bestmodel,TestSet_fertility)
table(svm.pred.test,TestSet_fertility$V10)
fertility.pred.test.acc = mean(svm.pred.test==TestSet_fertility$V10)*100
fertility.pred.test.acc
#fertility.pred.test.acc is 96.9697.



fertility.pred.test.err = 100-fertility.pred.test.acc
fertility.pred.test.err
#fertility.pred.test.err is 3.030303.



barplot(c(fertility.pred.train.acc,fertility.pred.test.err,fertility.pred.test.acc,fertility.pred.test.err),ylim = c(0,100),main = "SVM (poly Kernel) Method - fertility_Dataset ",ylab ="Percentages",col=c("blue","red"),names.arg =c("Training Accuracy","Training Error Rate","Test Accuracy","Test Error Rate"),legend=c("Accuracy","Error Rate"),args.legend = list(title="Percentages",x="topright",cex=0.6))
		
# Traning accuracy low than test accuracy which are indicated by blue bar, while tranning error rate and test error rate are same which indicated in red bar.

#----------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------
#                                                     END 





