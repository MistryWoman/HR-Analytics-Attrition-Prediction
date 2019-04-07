#Decision Tree
dataset=Book1

#Taking care of categorical variables
dataset$Attrition = factor(dataset$Attrition,
                           levels=c('Yes','No'),
                           labels=c(1,0))
dataset$Gender = factor(dataset$Gender,
                        levels= c('Female','Male'),
                        labels = c(1,0))
dataset$MaritalStatus= factor(dataset$MaritalStatus,
                              levels= c('Single','Married','Divorced'),
                              labels = c(1,2,3))
dataset$EducationField = factor(dataset$EducationField,
                                levels= c('Human Resources','Life Sciences','Marketing','Medical','Other','Technical Degree'),
                                labels= c(1,2,3,4,5,6))
dataset$BusinessTravel = factor(dataset$BusinessTravel,
                                levels=c('Non_Travel','Travel_Frequently','Travel_Rarely'),
                                labels= c(1,2,3))
dataset$OverTime = factor (dataset$OverTime,
                           levels=c('Yes','No'),
                           labels=c(1,0))

#Taking care of missing data in business travel
dataset$BusinessTravel[is.na(dataset$BusinessTravel)]=1

#Splitting into test and train sets
X=dataset[,1]
Y=dataset[,-1]
library(caTools)
set.seed(123)
split = sample.split(dataset$Attrition, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#Feature Scaling
#training_set[,c(-1,-2,-4,-5,-6,-11,-14)]= scale(training_set[,c(-1,-2,-4,-5,-6,-11,-14)])
#test_set[,c(-1,-2,-4,-5,-6,-11,-14)]= scale(test_set[,c(-1,-2,-4,-5,-6,-11,-14)])

#Fitting Support Vector Machines to Training Set
library(rpart)
classifier= rpart(formula = Attrition ~.,
                  data=training_set)

#Predicting test set results
y_pred=predict(classifier,newdata = test_set[-1],type='class')


#Adding new column of prediction to test set
test_set$Prediction=y_pred
write.csv(x=test_set,file="resultsdecisiontree1.CSV")

#Confusion Matrix
cm=table(unlist(test_set[,1]),unlist(y_pred))
