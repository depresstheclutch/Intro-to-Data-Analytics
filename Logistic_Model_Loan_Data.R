
#Read data file
Loan_data <- read.csv("TVS_Updated.csv")

#Find the structure of data
str(Loan_data)

#Removing the NA rows in the data set
na.omit(Loan_data)
Updated_loan_data <- na.omit(Loan_data)

#Change First EMI and Results variables from Interger to factor type
Updated_loan_data$First.EMI <- as.factor(Updated_loan_data$First.EMI)
Updated_loan_data$Result <- as.factor(Updated_loan_data$Result)

#Recheck the structure of data
str(Updated_loan_data)

#Partition Data - training with 80 percent data and testing with 20 percent data
set.seed(1)
Independent <- sample(2, nrow(Updated_loan_data), replace = T, prob = c(0.8,0.2))
Train <- Updated_loan_data[Independent==1,]
Test <- Updated_loan_data[Independent==2,]

# Logistic Regression Model with all the variables from the data set
logistic_model1 <- glm(Result ~ .,
                       data = Train, family = binomial(link = "logit"))

#Review logistic regression model results
summary(logistic_model1)
coef(logistic_model1)

#Calculate odds ratios
exp(coef(logistic_model1))

#Goodness of the model fit test
with(logistic_model1, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))

#Calculate McFadden's pseudo R squared
rsquare <- 1 - (logistic_model1$deviance / logistic_model1$null.deviance)
rsquare

#Calculate variance inflation factors
library(car)
vif(logistic_model1)

# Logistic Regression Model after removing the variable Resident_type because its not statically significant  
logistic_model2 <- glm(Result ~ First.EMI + Number.of.times.bounced.in.12.months + Months.of.business.with.TVS + EMI + Loan.Amount + No.of.advanced.EMI.s.paid
                        + Age.at.which.customer.has.taken.the.loan + Number.of.loans + Number.of.times.90.days.past.due.in.last.3.months,
                      data = Train, family = binomial(link = "logit"))

#Review logistic regression model results
summary(logistic_model2)

#Compare reduced model to original model
pchisq(logistic_model2$deviance - logistic_model1$deviance,
       logistic_model2$df.residual - logistic_model1$df.residual, lower = FALSE)

#Calculate McFadden's pseudo R squared
rsquare <- 1 - (logistic_model2$deviance / logistic_model2$null.deviance)
rsquare

#Prediction
pred1 <- predict(logistic_model2, Train, type = 'response')
pred1
#Create a confusion matrix for training data
logitpredictions = rep(0,length(pred1))
logitpredictions[pred1 > 0.5] <- 1
logitpredictions
table(logitpredictions, Train$Result)

#Check first few values of prediction results
head(pred1)
head(Train)

#Percentage of misclassification error
1 - sum(diag(Tab1))/sum(Tab1)

#Prediction
pred2 <- predict(logistic_model2, Test, type = 'response')
pred2
#Create a confusion matrix for test data
logitpredictions = rep(0,length(pred2))
logitpredictions1[pred2 > 0.5] <- 1
logitpredictions1
table(logitpredictions1, Test$Result)

#Check first few values of prediction results
head(pred2)
head(Test)

#Percentage of misclassification error
1 - sum(diag(Tab2))/sum(Tab2)

#Misclassification rate
p <- predict(logistic_model2, Updated_loan_data)
tab <- table(p, Updated_loan_data$Result)
tab

#Model Performance Evaluation
library(ROCR)
roc_prediction <- prediction(pred1,Train$Result)
roc_performance <- performance(roc_prediction, "tpr", "fpr")
roc_performance_auc <- performance(roc_prediction, "auc")
plot(roc_performance,col="#7842f5", lwd=3.0, main = paste("Area under the curve:", round(roc_performance_auc@y.values[[1]], 4)))

#Assess test model performance
tp <- length(which((logitpredictions1 == 1) & (Updated_loan_data$Result == 1)))
tn <- length(which((logitpredictions1 == 0) & (Updated_loan_data$Result == 0)))
fp <- length(which((logitpredictions1 == 1) & (Updated_loan_data$Result == 0)))
fn <- length(which((logitpredictions1 == 0) & (Updated_loan_data$Result == 1)))
logitaccuracy <- (tp+tn)/(tp+tn+fp+fn)
logitsensitivity <- tp/(tp+fn)
logitspecificity <- tn/(tn+fp)
logitprecision <- tp/(tp+fp)
logitaccuracy
logitsensitivity
logitspecificity
logitprecision

