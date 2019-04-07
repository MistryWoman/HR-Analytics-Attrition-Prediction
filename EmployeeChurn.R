# Load an R data frame.
MFG10YearTerminationData <- read.csv("~/Visual Studio 2015/Projects/EmployeeChurn/EmployeeChurn/MFG10YearTerminationData.csv")
MYdataset <- MFG10YearTerminationData
str(MYdataset)
library(plyr)
library(dplyr)

#data quality
summary(MYdataset)

#initial look
StatusCount<- as.data.frame.matrix(MYdataset %>%
                                     group_by(STATUS_YEAR) %>%
                                     select(STATUS) %>%
                                     table())
StatusCount$TOTAL<-StatusCount$ACTIVE + StatusCount$TERMINATED
StatusCount$PercentTerminated <-StatusCount$TERMINATED/(StatusCount$TOTAL)*100
StatusCount
mean(StatusCount$PercentTerminated)

library(ggplot2)
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(BUSINESS_UNIT),fill = as.factor(STATUS)),data=MYdataset,position = position_stack())


#just terminates
TerminatesData<- as.data.frame(MYdataset %>%
                                 filter(STATUS=="TERMINATED"))

ggplot() + geom_bar(aes(y = ..count..,x =as.factor(STATUS_YEAR),fill = as.factor(termtype_desc)),data=TerminatesData,position = position_stack())


ggplot() + geom_bar(aes(y = ..count..,x =as.factor(STATUS_YEAR),fill = as.factor(termreason_desc)),data=TerminatesData, position = position_stack())

ggplot() + geom_bar(aes(y = ..count..,x =as.factor(department_name),fill = as.factor(termreason_desc)),data=TerminatesData,position = position_stack())+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

library(caret)
featurePlot(x=MYdataset[,6:7],y=MYdataset$STATUS,plot="density",auto.key = list(columns = 2))

featurePlot(x=MYdataset[,6:7],y=MYdataset$STATUS,plot="box",auto.key = list(columns = 2))

#parition data

#Let's Partition The Data


library(rattle)   
library(magrittr) # For the %>% and %<>% operators.



building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 


# Load an R data frame.
MFG10YearTerminationData <- read.csv("~/Visual Studio 2015/Projects/EmployeeChurn/EmployeeChurn/MFG10YearTerminationData.csv")

MYdataset <- MFG10YearTerminationData



#Create training and testing datasets

set.seed(crv$seed) 
MYnobs <- nrow(MYdataset) # 52692 observations 
MYsample <- MYtrain <- subset(MYdataset,STATUS_YEAR<=2014)
MYvalidate <- NULL
MYtest <- subset(MYdataset,STATUS_YEAR== 2015)

# The following variable selections have been noted.

MYinput <- c("age", "length_of_service",    "gender_full",
               "STATUS_YEAR", "BUSINESS_UNIT")

MYnumeric <- c("age", "length_of_service", "STATUS_YEAR")

MYcategoric <- c(  
                   "gender_full", "BUSINESS_UNIT")

MYtarget  <- "STATUS"
MYrisk    <- NULL
MYident   <- "EmployeeID"
MYignore  <- c("recorddate_key", "birthdate_key", "orighiredate_key", "terminationdate_key", "city_name", "gender_short", "termreason_desc", "termtype_desc","department_name",
               "job_title", "store_name")
MYweights <- NULL

MYTrainingData<-MYtrain[c(MYinput, MYtarget)]
MYTestingData<-MYtest[c(MYinput, MYtarget)]

#============================================================
# Classification Models
# Decision Tree 

# The 'rpart' package provides the 'rpart' function.
library(rattle)
library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

MYrpart <- rpart(STATUS ~ .,
                 data=MYTrainingData,
                 method="class",
                 parms=list(split="information"),
                 control=rpart.control(usesurrogate=0, 
                                       maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

#print(MYrpart)
#printcp(MYrpart)
#cat("\n")

# Time taken: 0.63 secs

#============================================================
# Rattle timestamp: 2016-03-25 09:45:25 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(MYrpart, main="Decision Tree MFG10YearTerminationData $ STATUS")



#============================================================
# Rattle timestamp: 2016-03-25 18:21:29 x86_64-w64-mingw32 

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(crv$seed)
MYrf <- randomForest::randomForest(STATUS ~ .,
                                   data=MYtrain[c(MYinput, MYtarget)],
                                   ntree=500,
                                   mtry=2,
                                   importance=TRUE,
                                   na.action=randomForest::na.roughfix,
                                   replace=FALSE)

# Generate textual output of 'Random Forest' model.

MYrf

# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(MYrf$y, as.numeric(MYrf$predicted))

# Calculate the AUC Confidence Interval.

pROC::ci.auc(MYrf$y, as.numeric(MYrf$predicted))

# List the importance of the variables.

rn <- round(randomForest::importance(MYrf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 18.66 secs

#============================================================
# Rattle timestamp: 2016-03-25 18:22:22 x86_64-w64-mingw32 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
MYada <- ada::ada(STATUS ~ .,
                  data=MYtrain[c(MYinput, MYtarget)],
                  control=rpart::rpart.control(maxdepth=30,
                                               cp=0.010000,
                                               minsplit=20,
                                               xval=10),
                  iter=50)

# Print the results of the modelling.

print(MYada)
round(MYada$model$errs[MYada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(MYada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(MYada))

# Time taken: 27.73 secs

#============================================================
# Rattle timestamp: 2016-03-25 18:22:56 x86_64-w64-mingw32 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
MYksvm <- ksvm(as.factor(STATUS) ~ .,
               data=MYtrain[c(MYinput, MYtarget)],
               kernel="rbfdot",
               prob.model=TRUE)

# Generate a textual view of the SVM model.

MYksvm

# Time taken: 42.91 secs

#============================================================
# Rattle timestamp: 2016-03-25 18:23:56 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

MYglm <- glm(STATUS ~ .,
             data=MYtrain[c(MYinput, MYtarget)],
             family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(MYglm))
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(MYglm)[1],
            attr(logLik(MYglm), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            MYglm$null.deviance-MYglm$deviance,
            MYglm$df.null-MYglm$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(MYglm$null.deviance-MYglm$deviance,
                   MYglm$df.null-MYglm$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
            cor(MYglm$y, MYglm$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(MYglm, test="Chisq"))
cat("\n")

# Time taken: 1.62 secs

#============================================================
# Rattle timestamp: 2016-03-25 18:50:22 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

MYpr <- predict(MYrpart, newdata=MYtest[c(MYinput, MYtarget)], type="class")

# Generate the confusion matrix showing counts.

table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Ada Boost model.

# Obtain the response from the Ada Boost model.

MYpr <- predict(MYada, newdata=MYtest[c(MYinput, MYtarget)])

# Generate the confusion matrix showing counts.

table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

MYpr <- predict(MYrf, newdata=na.omit(MYtest[c(MYinput, MYtarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS, MYpr,
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS, MYpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

MYpr <- kernlab::predict(MYksvm, newdata=na.omit(MYtest[c(MYinput, MYtarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS, MYpr,
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS, MYpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

MYpr <- as.vector(ifelse(predict(MYglm, type="response", newdata=MYtest[c(MYinput, MYtarget)]) > 0.5, "TERMINATED", "ACTIVE"))

# Generate the confusion matrix showing counts.

table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2016-03-25 19:44:22 x86_64-w64-mingw32 

# Evaluate model performance. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rpart model on MFG10YearTerminationData [test].

MYpr <- predict(MYrpart, newdata=MYtest[c(MYinput, MYtarget)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Decision Tree MFG10YearTerminationData [test] STATUS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ada model on MFG10YearTerminationData [test].

MYpr <- predict(MYada, newdata=MYtest[c(MYinput, MYtarget)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Ada Boost MFG10YearTerminationData [test] STATUS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the rf model on MFG10YearTerminationData [test].

MYpr <- predict(MYrf, newdata=na.omit(MYtest[c(MYinput, MYtarget)]), type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Random Forest MFG10YearTerminationData [test] STATUS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ksvm model on MFG10YearTerminationData [test].

MYpr <- kernlab::predict(MYksvm, newdata=na.omit(MYtest[c(MYinput, MYtarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve SVM MFG10YearTerminationData [test] STATUS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the glm model on MFG10YearTerminationData [test].

MYpr <- predict(MYglm, type="response", newdata=MYtest[c(MYinput, MYtarget)])

# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Linear MFG10YearTerminationData [test] STATUS")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
performance(pred, "auc")

#============================================================
# Rattle timestamp: 2016-03-25 19:45:50 x86_64-w64-mingw32 

# Evaluate model performance. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the rpart model on MFG10YearTerminationData [test].

MYpr <- predict(MYrpart, newdata=MYtest[c(MYinput, MYtarget)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ada model on MFG10YearTerminationData [test].

MYpr <- predict(MYada, newdata=MYtest[c(MYinput, MYtarget)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#A3CC00FF", lty=2, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the rf model on MFG10YearTerminationData [test].

MYpr <- predict(MYrf, newdata=na.omit(MYtest[c(MYinput, MYtarget)]), type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#00CC52FF", lty=3, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ksvm model on MFG10YearTerminationData [test].

MYpr <- kernlab::predict(MYksvm, newdata=na.omit(MYtest[c(MYinput, MYtarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#0052CCFF", lty=4, add=TRUE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the glm model on MFG10YearTerminationData [test].

MYpr <- predict(MYglm, type="response", newdata=MYtest[c(MYinput, MYtarget)])

# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#A300CCFF", lty=5, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("rpart","ada","rf","ksvm","glm"), col=rainbow(5, 1, .8), lty=1:5, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  MFG10YearTerminationData [test]",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#============================================================
# Rattle timestamp: 2016-03-25 19:46:45 x86_64-w64-mingw32 

# Evaluate model performance. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for rpart model on MFG10YearTerminationData [test].

MYpr <- predict(MYrpart, newdata=MYtest[c(MYinput, MYtarget)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ada model on MFG10YearTerminationData [test].

MYpr <- predict(MYada, newdata=MYtest[c(MYinput, MYtarget)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#A3CC00FF", lty=2, add=TRUE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for rf model on MFG10YearTerminationData [test].

MYpr <- predict(MYrf, newdata=na.omit(MYtest[c(MYinput, MYtarget)]), type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#00CC52FF", lty=3, add=TRUE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ksvm model on MFG10YearTerminationData [test].

MYpr <- kernlab::predict(MYksvm, newdata=na.omit(MYtest[c(MYinput, MYtarget)]), type="probabilities")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(na.omit(MYtest[c(MYinput, MYtarget)])$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#0052CCFF", lty=4, add=TRUE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for glm model on MFG10YearTerminationData [test].

MYpr <- predict(MYglm, type="response", newdata=MYtest[c(MYinput, MYtarget)])

# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#A300CCFF", lty=5, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("rpart","ada","rf","ksvm","glm"), col=rainbow(5, 1, .8), lty=1:5, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  MFG10YearTerminationData [test]",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()


#Apply model
#Generate 2016 data
Employees2016<-MYtest #2015 data

ActiveEmployees2016<-subset(Employees2016,STATUS=='ACTIVE')
ActiveEmployees2016$age<-ActiveEmployees2016$age+1
ActiveEmployees2016$length_of_service<-ActiveEmployees2016$length_of_service+1

#Predict 2016 Terminates using adaboost
ActiveEmployees2016$PredictedSTATUS2016<-predict(MYada,ActiveEmployees2016)
PredictedTerminatedEmployees2016<-subset(ActiveEmployees2016,PredictedSTATUS2016=='TERMINATED')
#show records for first 5 predictions
head(PredictedTerminatedEmployees2016)
