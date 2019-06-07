# 03.05.2019 
# Matthew Dobbin
# Northwestern University
# MSDS 498 Capstone Project

#Read in modeling dataframe.
#f_default <- readRDS(file = 'C:/Users/dobbin/Desktop/Northwestern/498 Capstone/Model/f_default.rds')
f_default <- readRDS(file = "C:/Users/mkdob/OneDrive/Northwestern/498 Capstone/Model/f_default.rds")


library(caret) #Required for confusion table normalization and gbm control
library(pROC) #Required for ROC curve
par(mfrow=c(1,2))

########################################################################
########################################################################

# PREDICTIVE MODELLING

# A variety of modelling approaches have been implemented. They are:
#   1) Random Forest,
#   2) Logistic Regression Junk Model 
#   3) Logistic Regression with Step AIC variable selection
#   4) Gradient Boosting
#   5) Naive Bayes
#   6) SVM - Discrete
#   7) SVM - Continous
# 

#The following performance metrics have been used to evaluate in sample (i.e on the training data set)
# and out-of-sample (i.e on the test data set):
#   1) true positive rate or sensitivity,
#   2) false positive rate
#   3) the accuracy


#####################################
#
#Subset into train, test, and validate dataframes. 
train_rf <- subset(f_default, data.group ==1)
test_rf <- subset(f_default, data.group ==2)

#RANDOM FOREST
library(randomForest)

# Drop data.group from training dataframe
train_rf <- subset(train_rf, select = -c(data.group))
str(train_rf) #mtry 46 

# Train model as well as calculate time to train.
set.seed(1)
start.time <- Sys.time()
rf_model <- randomForest(DEFAULT ~ ., data = train_rf, mtry = 46, importance=TRUE)
rf_model

end.time = Sys.time()
rf_model.taken <- end.time - start.time
rf_model.taken

#Create variables of importance plot.
importance(rf_model)
varImpPlot(rf_model)
rf_model

#######################################################################
#IN SAMPLE MODEL PERFORMANCE
#Predicted values for training data set
in_model_rf_score <- predict(rf_model, train_rf, type="prob")[,2]

#Create ROC curve for training sample
in_roc_rf <- roc(response=train_rf$DEFAULT, predictor=in_model_rf_score)
print(in_roc_rf)

# Compute AUC
in_auc_rf <- auc(in_roc_rf)
in_auc_rf

#Find the threshold value recommended by the ROC curve
coords(roc=in_roc_rf,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

in_roc_rf.specs <- coords(roc=in_roc_rf,x=c('best'),
                    input=c('threshold','specificity','sensitivity'),
                    ret=c('threshold','specificity','sensitivity'),
                    as.list=TRUE)
in_roc_rf.specs

# Add predicted values to the train dataframe
train_rf$in_modelscores <- in_model_rf_score

#Use the threshold to assign the classes
train_rf$in_classes <- ifelse(train_rf$in_modelscores > in_roc_rf.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(train_rf$DEFAULT, train_rf$in_classes)
t
#Normalize confusion matrix into rates.

prop.table(t)

#Accuracy = (TN + TP) / (TP+TN + FN + FP)
in_rf_acc <- (0.74835310 + 0.19301713) / (0.74835310 + 0.19301713 + 0.03247694 + 0.02615283)
in_rf_acc
mean(train_rf$in_classes==train_rf$DEFAULT)

#True Positive Rate (Sensitivity) = TP / (TP +FN)
in_rf_TPR <- 0.19301713 /(0.19301713 + 0.03247694)
in_rf_TPR
in_rf_TPR <- in_roc_rf.specs$sensitivity
in_rf_TPR
#False Positive Rate = FP/(FP/TN)
in_rf_FPR <- 0.02615283 /(0.02615283 + 0.74835310)
in_rf_FPR
in_rf_FPR <- 1-in_roc_rf.specs$specificity
in_rf_FPR
#Manually calculated values from confusion matrix match function values. 
#Will no longer manually calculate them.

#######################################################################
# OUT oF SAMPLE MODEL PERFORMANCE

#Prediction porbabilities on the test set.
out_model_rf_score <- predict(rf_model, test_rf, type="prob")[,2]

#Create ROC curve for test sample
out_roc_rf <- roc(response=test_rf$DEFAULT, predictor=out_model_rf_score)
print(out_roc_rf)
plot(out_roc_rf)

# Compute AUC
out_auc_rf <- auc(out_roc_rf)
out_auc_rf

#Find the threshold value recommended by the ROC curve
coords(roc=out_roc_rf,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

out_roc_rf.specs <- coords(roc=out_roc_rf,x=c('best'),
                    input=c('threshold','specificity','sensitivity'),
                    ret=c('threshold','specificity','sensitivity'),
                    as.list=TRUE)
out_roc_rf.specs

# Add predicted values to the test dataframe
test_rf$out_modelscores <- out_model_rf_score

#Use the threshold to assign the classes
test_rf$out_classes <- ifelse(test_rf$out_modelscores > out_roc_rf.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(test_rf$DEFAULT, test_rf$out_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Check confusion matrix set out correctly
out_rf_acc <- mean(test_rf$out_classes==test_rf$DEFAULT)
out_rf_TPR <- out_roc_rf.specs$sensitivity
out_rf_FPR <- 1- out_roc_rf.specs$specificity
out_rf_acc  
out_rf_TPR 
out_rf_FPR 

#Create ROC plots
par(mfrow=c(1,2))
plot(in_roc_rf, main = "In Sample ROC Curve AUC = 0.94", cex.main=0.9)
plot(out_roc_rf, main = "Out of Sample ROC Curve AUC = 0.73", cex.main=0.9)



################################################################################
#Gradient Boosting GBM
################################################################################
train_gb <- subset(f_default, data.group ==1)
train_gb <- subset(train_gb, select = -c(data.group))
test_gb <- subset(f_default, data.group ==2)

#Gradient boosted model using caret packet for grid search to find optimal parameters
library(gbm)
Control <- trainControl(method = "repeatedcv", number =5, repeats = 5)
set.seed(1)

start.time <- Sys.time()

model_gb <- train(DEFAULT ~ PAY_R1_G1 + SEX_M + AGE_26_40 + MAX_DLQ_G1 + EDUCATION_UNI 
                  + MAX_PAY_AMT_5000_36621 + AVG_PMT_RATIO_46_1 + BAL_GROWTH_6MO_N10172_923 
                  + MARRIAGE_MD + MARRIAGE_SN + EDUCATION_GS + AGE_18_25 + EDUCATION_HS
                  + UTIL_GROWTH_6MO_LEN21 + MAX_BILL_AMT_G52496 + AVG_PMT_RATIO_46
                  + AVG_PAY_AMT_LE2000 + MAX_BILL_AMT_4079_18400 + UTIL_SEP_8_14 + AVG_UTIL_16_22
                  + AVG_UTIL_22_28 + PAY_R1_LE0 + PAY_R3_G0 + UTIL_AUG_18_21 + AVG_BILL_AMT_7373_31478
                  + MAX_BILL_AMT_18400_21034 + PAY_R2_G0 + UTIL_AUG_G24 + AVG_BILL_AMT_LE697
                  + PAY_R5_G0 + PAY_R6_G0 + AVG_UTIL_G28 + MAX_BILL_AMT_600_4079 + MAX_BILL_AMT_LE600,
                  data = train_gb, method = "gbm", trControl = Control, verbose = FALSE, tuneLength=5)

end.time = Sys.time()
model_gb.taken <- end.time - start.time
model_gb.taken

model_gb
summary(model_gb)

#Variable Importance
varImp(object=model_gb)
#Plotting Varianle importance for GBM
plot(varImp(object=model_gb),main="GBM - Variable Importance")

#######################################################################
#IN SAMPLE MODEL PERFORMANCE
#Predicted values for training data set ()
in_model_gb_score <- predict(model_gb, newdata=train_gb, n.trees=50 , type="prob")[,2] 

#Create ROC curve for training sample
in_roc_gb <- roc(response=train_gb$DEFAULT, predictor=in_model_gb_score)
print(in_roc_gb)

# Compute AUC
in_auc_gb <- auc(in_roc_gb)
in_auc_gb

#Find the threshold value recommended by the ROC curve
coords(roc=in_roc_gb,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

in_roc_gb.specs <- coords(roc=in_roc_gb,x=c('best'),
                          input=c('threshold','specificity','sensitivity'),
                          ret=c('threshold','specificity','sensitivity'),
                          as.list=TRUE)
in_roc_gb.specs

# Add predicted values to the train dataframe
train_gb$in_modelscores <- in_model_gb_score

#Use the threshold to assign the classes
train_gb$in_classes <- ifelse(train_gb$in_modelscores > in_roc_gb.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(train_gb$DEFAULT, train_gb$in_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Calculate accuracy, TPR, FPR
in_gb_acc <- mean(train_gb$in_classes==train_gb$DEFAULT)
in_gb_acc
in_gb_TPR <- in_roc_gb.specs$sensitivity
in_gb_TPR
in_gb_FPR <- 1-in_roc_gb.specs$specificity
in_gb_FPR

#######################################################################
# OUT oF SAMPLE MODEL PERFORMANCE

#Prediction porbabilities on the test set.
out_model_gb_score <- predict(model_gb, newdata=test_gb, n.trees=50 , type="prob")[,2] 

#Create ROC curve for test sample
out_roc_gb <- roc(response=test_gb$DEFAULT, predictor=out_model_gb_score)
print(out_roc_gb)
plot(out_roc_gb)

# Compute AUC
out_auc_gb <- auc(out_roc_gb)
out_auc_gb

#Find the threshold value recommended by the ROC curve
coords(roc=out_roc_gb,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

out_roc_gb.specs <- coords(roc=out_roc_gb,x=c('best'),
                           input=c('threshold','specificity','sensitivity'),
                           ret=c('threshold','specificity','sensitivity'),
                           as.list=TRUE)
out_roc_gb.specs

# Add predicted values to the test dataframe
test_gb$out_modelscores <- out_model_gb_score

#Use the threshold to assign the classes
test_gb$out_classes <- ifelse(test_gb$out_modelscores > out_roc_gb.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(test_gb$DEFAULT, test_gb$out_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Check confusion matrix set out correctly
out_gb_acc <- mean(test_gb$out_classes==test_gb$DEFAULT)
out_gb_TPR <- out_roc_gb.specs$sensitivity
out_gb_FPR <- 1- out_roc_gb.specs$specificity
out_gb_acc  
out_gb_TPR 
out_gb_FPR 

#Create ROC plots
par(mfrow=c(1,2))
plot(in_roc_gb, main = "In Sample ROC Curve AUC = 0.78", cex.main=0.9)
plot(out_roc_gb, main = "Out of Sample ROC Curve AUC = 0.78", cex.main=0.9)


#######################################################################
#
#Logistic regression (junk model)
#######################################################################
train_lr <- subset(f_default, data.group ==1)
train_lr <- subset(train_lr, select = -c(data.group))
test_lr <- subset(f_default, data.group ==2)

set.seed(1)
start.time <- Sys.time()

#Fit model
model_lr <- glm(DEFAULT ~ PAY_R1_G1 + SEX_M + AGE_26_40, data=train_lr, family='binomial')
summary(model_lr)

end.time = Sys.time()
lr_model.taken <- end.time - start.time
lr_model.taken

#######################################################################
#IN SAMPLE MODEL PERFORMANCE
#Predicted values for training data set
in_model_lr_score <- predict(model_lr, train_lr, type = "response")

#Create ROC curve for training sample
in_roc_lr <- roc(response=train_lr$DEFAULT, predictor=in_model_lr_score)
print(in_roc_lr)

# Compute AUC
in_auc_lr <- auc(in_roc_lr)
in_auc_lr

#Find the threshold value recommended by the ROC curve
coords(roc=in_roc_lr,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

in_roc_lr.specs <- coords(roc=in_roc_lr,x=c('best'),
                       input=c('threshold','specificity','sensitivity'),
                       ret=c('threshold','specificity','sensitivity'),
                       as.list=TRUE)
in_roc_lr.specs

# Add predicted values to the train dataframe
train_lr$in_modelscores <- in_model_lr_score

#Use the threshold to assign the classes
train_lr$in_classes <- ifelse(train_lr$in_modelscores > in_roc_lr.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(train_lr$DEFAULT, train_lr$in_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Calculate accuracy, TPR, FPR
in_lr_acc <- mean(train_lr$in_classes==train_lr$DEFAULT)
in_lr_acc
in_lr_TPR <- in_roc_lr.specs$sensitivity
in_lr_TPR
in_lr_FPR <- 1-in_roc_lr.specs$specificity
in_lr_FPR

#######################################################################
# OUT oF SAMPLE MODEL PERFORMANCE

#Prediction porbabilities on the test set.
out_model_lr_score <- predict(model_lr, test_lr, type = "response")

#Create ROC curve for test sample
out_roc_lr <- roc(response=test_lr$DEFAULT, predictor=out_model_lr_score)
print(out_roc_lr)
plot(out_roc_lr)

# Compute AUC
out_auc_lr <- auc(out_roc_lr)
out_auc_lr

#Find the threshold value recommended by the ROC curve
coords(roc=out_roc_lr,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

out_roc_lr.specs <- coords(roc=out_roc_lr,x=c('best'),
                        input=c('threshold','specificity','sensitivity'),
                        ret=c('threshold','specificity','sensitivity'),
                        as.list=TRUE)
out_roc_lr.specs

# Add predicted values to the test dataframe
test_lr$out_modelscores <- out_model_lr_score

#Use the threshold to assign the classes
test_lr$out_classes <- ifelse(test_lr$out_modelscores > out_roc_lr.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(test_lr$DEFAULT, test_lr$out_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Check confusion matrix set out correctly
out_lr_acc <- mean(test_lr$out_classes==test_lr$DEFAULT)
out_lr_TPR <- out_roc_lr.specs$sensitivity
out_lr_FPR <- 1- out_roc_lr.specs$specificity
out_lr_acc  
out_lr_TPR 
out_lr_FPR 

#Create ROC plots
par(mfrow=c(1,2))
plot(in_roc_lr, main = "In Sample ROC Curve AUC = 0.67", cex.main=0.9)
plot(out_roc_lr, main = "Out of Sample ROC Curve AUC = 0.67", cex.main=0.9)


#######################################################################
#
#LOGISTIC REGRESSION with backward selection
library(MASS)
train_blr <- subset(f_default, data.group ==1)
train_blr <- subset(train_blr, select = -c(data.group))
test_blr <- subset(f_default, data.group ==2)

#Issues with step AIC and DEFAULT being a factor. Change to numeric.
train_blr$DEFAULT <- as.numeric(train_blr$DEFAULT)-1
table(train_blr$DEFAULT)
test_blr$DEFAULT <- as.numeric(test_blr$DEFAULT)-1
table(test_blr$DEFAULT)

#Fitted a logistic regression model with backward variable selection
start.time <- Sys.time()
model_blr <- list()

# Define the upper model as the model with pool of influential variables from GBM 
upper_lr <- glm(DEFAULT ~ PAY_R1_G1 + MAX_DLQ_G1  + PAY_R1_LE0  + AVG_PAY_AMT_LE2000  + 
               PAY_R5_G0  + PAY_R3_G0 + PAY_R2_G0  + AVG_UTIL_16_22  +
               MAX_BILL_AMT_LE600  + AVG_BILL_AMT_LE697  + BAL_GROWTH_6MO_N10172_923 +
               PAY_R6_G0  + MARRIAGE_MD  + UTIL_AUG_18_21  + MAX_BILL_AMT_600_4079  +
               UTIL_AUG_G24 + AVG_PMT_RATIO_46_1 + MAX_BILL_AMT_4079_18400  + 
               SEX_M  + AVG_PMT_RATIO_46  + MARRIAGE_SN, data=train_blr, family='binomial') 
summary(upper_lr) 

#Call stepAIC() for variable selection
model_blr <- stepAIC(object=upper_lr,direction=c('backward')); 
summary(model_blr)

end.time = Sys.time()
model_blr.taken <- end.time - start.time
model_blr.taken

#Compute VIF values
library(car)
sort(vif(model_blr), decreasing=TRUE)

#######################################################################
#IN SAMPLE MODEL PERFORMANCE
#Predicted values for training data set
in_model_blr_score <- predict(model_blr, train_blr, type = "response")

#Create ROC curve for training sample
in_roc_blr <- roc(response=train_blr$DEFAULT, predictor=in_model_blr_score)
print(in_roc_blr)

# Compute AUC
in_auc_blr <- auc(in_roc_blr)
in_auc_blr

#Find the threshold value recommended by the ROC curve
coords(roc=in_roc_blr,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

in_roc_blr.specs <- coords(roc=in_roc_blr,x=c('best'),
                       input=c('threshold','specificity','sensitivity'),
                       ret=c('threshold','specificity','sensitivity'),
                       as.list=TRUE)
in_roc_blr.specs

# Add predicted values to the train dataframe
train_blr$in_modelscores <- in_model_blr_score

#Use the threshold to assign the classes
train_blr$in_classes <- ifelse(train_blr$in_modelscores > in_roc_blr.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(train_blr$DEFAULT, train_blr$in_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Calculate accuracy, TPR, FPR
in_blr_acc <- mean(train_blr$in_classes==train_blr$DEFAULT)
in_blr_acc
in_blr_TPR <- in_roc_blr.specs$sensitivity
in_blr_TPR
in_blr_FPR <- 1-in_roc_blr.specs$specificity
in_blr_FPR

#######################################################################
# OUT oF SAMPLE MODEL PERFORMANCE

#Prediction porbabilities on the test set.
out_model_blr_score <- predict(model_blr, test_blr, type = "response")

#Create ROC curve for test sample
out_roc_blr <- roc(response=test_blr$DEFAULT, predictor=out_model_blr_score)
print(out_roc_blr)
plot(out_roc_blr)

# Compute AUC
out_auc_blr <- auc(out_roc_blr)
out_auc_blr

#Find the threshold value recommended by the ROC curve
coords(roc=out_roc_blr,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

out_roc_blr.specs <- coords(roc=out_roc_blr,x=c('best'),
                        input=c('threshold','specificity','sensitivity'),
                        ret=c('threshold','specificity','sensitivity'),
                        as.list=TRUE)
out_roc_blr.specs

# Add predicted values to the test dataframe
test_blr$out_modelscores <- out_model_blr_score

#Use the threshold to assign the classes
test_blr$out_classes <- ifelse(test_blr$out_modelscores > out_roc_blr.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(test_blr$DEFAULT, test_blr$out_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Check confusion matrix set out correctly
out_blr_acc <- mean(test_blr$out_classes==test_blr$DEFAULT)
out_blr_TPR <- out_roc_blr.specs$sensitivity
out_blr_FPR <- 1- out_roc_blr.specs$specificity
out_blr_acc  
out_blr_TPR 
out_blr_FPR 

#Create ROC plots
par(mfrow=c(1,2))
plot(in_roc_blr, main = "In Sample ROC Curve AUC = 0.78", cex.main=0.9)
plot(out_roc_blr, main = "Out of Sample ROC Curve AUC = 0.78", cex.main=0.9)



###############################################################################
#Naive Bayes
###############################################################################
library(naivebayes)
train_nb <- subset(f_default, data.group ==1)
test_nb <- subset(f_default, data.group ==2)


#Naive model built with the most influential predictors from the gradient boosted model.
#Need to seperate target from predictors.
ytrain <- train_nb$DEFAULT
ytest <- test_nb$DEFAULT
keep.vars <- keep.vars <- c('PAY_R1_G1', 'MAX_DLQ_G1' , 'PAY_R1_LE0' , 'AVG_PAY_AMT_LE2000' , 
                            'PAY_R5_G0' , 'PAY_R3_G0', 'PAY_R2_G0' , 'AVG_UTIL_16_22' ,
                            'MAX_BILL_AMT_LE600' , 'AVG_BILL_AMT_LE697' , 'BAL_GROWTH_6MO_N10172_923',
                            'PAY_R6_G0' , 'MARRIAGE_MD' , 'UTIL_AUG_18_21' , 'MAX_BILL_AMT_600_4079' ,
                            'UTIL_AUG_G24', 'AVG_PMT_RATIO_46_1', 'MAX_BILL_AMT_4079_18400' , 
                            'SEX_M' , 'AVG_PMT_RATIO_46' , 'MARRIAGE_SN')
train_nb <- train_nb[, keep.vars]

start.time <- Sys.time()
# Fit naive model
model_nb <- naive_bayes(x=train_nb , y=ytrain)

end.time = Sys.time()
model_nb.taken <- end.time - start.time
model_nb.taken

# Plot Naive Bayes probabilites for PAY_R1_G1 variable
plot(model_nb, which=c('PAY_R1_G1'))

#######################################################################
#IN SAMPLE MODEL PERFORMANCE
#Predicted values for training data set
in_model_nb_score <- predict(model_nb, newdata=train_nb, type=c('prob'))[,2]

#Create ROC curve for training sample
in_roc_nb <- roc(response=ytrain, predictor=in_model_nb_score)
print(in_roc_nb)

# Compute AUC
in_auc_nb <- auc(in_roc_nb)
in_auc_nb

#Find the threshold value recommended by the ROC curve
coords(roc=in_roc_nb,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

in_roc_nb.specs <- coords(roc=in_roc_nb,x=c('best'),
                           input=c('threshold','specificity','sensitivity'),
                           ret=c('threshold','specificity','sensitivity'),
                           as.list=TRUE)
in_roc_nb.specs

# Add predicted values to the train dataframe
train_nb$in_modelscores <- in_model_nb_score

#Use the threshold to assign the classes
train_nb$in_classes <- ifelse(train_nb$in_modelscores > in_roc_nb.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(ytrain, train_nb$in_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Calculate accuracy, TPR, FPR
in_nb_acc <- mean(train_nb$in_classes==ytrain)
in_nb_acc
in_nb_TPR <- in_roc_nb.specs$sensitivity
in_nb_TPR
in_nb_FPR <- 1-in_roc_nb.specs$specificity
in_nb_FPR

#######################################################################
# OUT oF SAMPLE MODEL PERFORMANCE

#Prediction porbabilities on the test set.
out_model_nb_score <- predict(model_nb, newdata=test_nb, type=c('prob'))[,2]

#Create ROC curve for test sample
out_roc_nb <- roc(response=ytest, predictor=out_model_nb_score)
print(out_roc_nb)
plot(out_roc_nb)

# Compute AUC
out_auc_nb <- auc(out_roc_nb)
out_auc_nb

#Find the threshold value recommended by the ROC curve
coords(roc=out_roc_nb,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

out_roc_nb.specs <- coords(roc=out_roc_nb,x=c('best'),
                            input=c('threshold','specificity','sensitivity'),
                            ret=c('threshold','specificity','sensitivity'),
                            as.list=TRUE)
out_roc_nb.specs

# Add predicted values to the test dataframe
test_nb$out_modelscores <- out_model_nb_score

#Use the threshold to assign the classes
test_nb$out_classes <- ifelse(test_nb$out_modelscores > out_roc_nb.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(ytest, test_nb$out_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Check confusion matrix set out correctly
out_nb_acc <- mean(test_nb$out_classes==ytest)
out_nb_TPR <- out_roc_nb.specs$sensitivity
out_nb_FPR <- 1- out_roc_nb.specs$specificity
out_nb_acc  
out_nb_TPR 
out_nb_FPR 

#Create ROC plots
par(mfrow=c(1,2))
plot(in_roc_nb, main = "In Sample ROC Curve AUC = 0.77", cex.main=0.9)
plot(out_roc_nb, main = "Out of Sample ROC Curve AUC = 0.77", cex.main=0.9)

################################################################################
# Naive bayes with search parameters #doesn't improve AUC value
################################################################################
library(h2o)
train_nb2 <- subset(f_default, data.group ==1)
test_nb2 <- subset(f_default, data.group ==2)


#Naive model built with the most influential predictors from the gradient boosted model.
#Need to seperate target from predictors.
ytrain <- train_nb2$DEFAULT
ytest <- test_nb2$DEFAULT
keep.vars <- keep.vars <- c('PAY_R1_G1', 'MAX_DLQ_G1' , 'PAY_R1_LE0' , 'AVG_PAY_AMT_LE2000' , 
                            'PAY_R5_G0' , 'PAY_R3_G0', 'PAY_R2_G0' , 'AVG_UTIL_16_22' ,
                            'MAX_BILL_AMT_LE600' , 'AVG_BILL_AMT_LE697' , 'BAL_GROWTH_6MO_N10172_923',
                            'PAY_R6_G0' , 'MARRIAGE_MD' , 'UTIL_AUG_18_21' , 'MAX_BILL_AMT_600_4079' ,
                            'UTIL_AUG_G24', 'AVG_PMT_RATIO_46_1', 'MAX_BILL_AMT_4079_18400' , 
                            'SEX_M' , 'AVG_PMT_RATIO_46' , 'MARRIAGE_SN')
train_nb2 <- train_nb2[, keep.vars]

# set up 10-fold cross validation procedure
train_control <- trainControl(method = "cv", number = 10)

# set up tuning grid
search_grid <- expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = seq(0, 5, by = 1))

#Fit model
model_nb2 <- train(x = train_nb, y = ytrain, method = "nb", trControl = train_control, tuneGrid = search_grid)

#######################################################################
#IN SAMPLE MODEL PERFORMANCE
#Predicted values for training data set
in_model_nb2_score <- predict(model_nb2, newdata=train_nb2, type=c('prob'))[,2]

#Create ROC curve for training sample
in_roc_nb2 <- roc(response=ytrain, predictor=in_model_nb2_score)
print(in_roc_nb2)

# Compute AUC
in_auc_nb2 <- auc(in_roc_nb2)
in_auc_nb2

#Find the threshold value recommended by the ROC curve
coords(roc=in_roc_nb2,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

in_roc_nb2.specs <- coords(roc=in_roc_nb2,x=c('best'),
                          input=c('threshold','specificity','sensitivity'),
                          ret=c('threshold','specificity','sensitivity'),
                          as.list=TRUE)
in_roc_nb2.specs

# Add predicted values to the train dataframe
train_nb2$in_modelscores <- in_model_nb2_score

#Use the threshold to assign the classes
train_nb2$in_classes <- ifelse(train_nb2$in_modelscores > in_roc_nb2.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(ytrain, train_nb2$in_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Calculate accuracy, TPR, FPR
in_nb2_acc <- mean(train_nb2$in_classes==ytrain)
in_nb2_acc
in_nb2_TPR <- in_roc_nb2.specs$sensitivity
in_nb2_TPR
in_nb2_FPR <- 1-in_roc_nb2.specs$specificity
in_nb2_FPR

#######################################################################
# OUT oF SAMPLE MODEL PERFORMANCE

#Prediction porbabilities on the test set.
out_model_nb2_score <- predict(model_nb2, newdata=test_nb2, type=c('prob'))[,2]

#Create ROC curve for test sample
out_roc_nb2 <- roc(response=ytest, predictor=out_model_nb2_score)
print(out_roc_nb2)
plot(out_roc_nb2)

# Compute AUC
out_auc_nb2 <- auc(out_roc_nb2)
out_auc_nb2

#Find the threshold value recommended by the ROC curve
coords(roc=out_roc_nb2,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

out_roc_nb2.specs <- coords(roc=out_roc_nb2,x=c('best'),
                           input=c('threshold','specificity','sensitivity'),
                           ret=c('threshold','specificity','sensitivity'),
                           as.list=TRUE)
out_roc_nb2.specs

# Add predicted values to the test dataframe
test_nb2$out_modelscores <- out_model_nb2_score

#Use the threshold to assign the classes
test_nb2$out_classes <- ifelse(test_nb2$out_modelscores > out_roc_nb2.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(ytest, test_nb2$out_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Check confusion matrix set out correctly
out_nb2_acc <- mean(test_nb2$out_classes==ytest)
out_nb2_TPR <- out_roc_nb2.specs$sensitivity
out_nb2_FPR <- 1- out_roc_nb2.specs$specificity
out_nb2_acc  
out_nb2_TPR 
out_nb2_FPR 

#Create ROC plots
par(mfrow=c(1,2))
plot(in_roc_nb2, main = "In Sample ROC Curve AUC = 0.77", cex.main=0.9)
plot(out_roc_nb2, main = "Out of Sample ROC Curve AUC = 0.77", cex.main=0.9)


###############################################################################
#Support Vector Machine - Discrete and continous variables that have been 
#                         binned into discrete variables 
################################################################################
library(e1071)
train_sv <- subset(f_default, data.group ==1)
train_sv <- subset(train_sv, select = -c(data.group))
test_sv <- subset(f_default, data.group ==2)

# Train model as well as calculate time to train.
set.seed(1)
start.time <- Sys.time()

#Use grid search to find optimal parameters 
svmt <- tune(svm, DEFAULT ~ PAY_R1_G1 + MAX_DLQ_G1 + PAY_R1_LE0 + AVG_PAY_AMT_LE2000 + PAY_R5_G0 + PAY_R3_G0
             + PAY_R2_G0 + AVG_UTIL_16_22 + MAX_BILL_AMT_LE600 + AVG_BILL_AMT_LE697 + BAL_GROWTH_6MO_N10172_923
             + PAY_R6_G0 + MARRIAGE_MD + UTIL_AUG_18_21 + MAX_BILL_AMT_600_4079 + UTIL_AUG_G24 
             + AVG_PMT_RATIO_46_1 + MAX_BILL_AMT_4079_18400 + SEX_M + AVG_PMT_RATIO_46 + MARRIAGE_SN
             , data=train_sv, probability=TRUE, kernel="radial", ranges=list(cost=c(1,10), gamma=c(0.5,1)))


#model_sv <- svm(DEFAULT ~ PAY_R1_G1 + MAX_DLQ_G1 + PAY_R1_LE0 + AVG_PAY_AMT_LE2000 + PAY_R5_G0 + PAY_R3_G0
               # + PAY_R2_G0 + AVG_UTIL_16_22 + MAX_BILL_AMT_LE600 + AVG_BILL_AMT_LE697 + BAL_GROWTH_6MO_N10172_923
                #+ PAY_R6_G0 + MARRIAGE_MD + UTIL_AUG_18_21 + MAX_BILL_AMT_600_4079 + UTIL_AUG_G24 
                #+ AVG_PMT_RATIO_46_1 + MAX_BILL_AMT_4079_18400 + SEX_M + AVG_PMT_RATIO_46 + MARRIAGE_SN,
                #  data = train_sv, kernel="radial", cost = 1, gamma= 0.5, probability=TRUE)

end.time = Sys.time()
model_sv.taken <- end.time - start.time
model_sv.taken

model_sv <- svmt$best.model

model_sv
summary(model_sv)


#######################################################################
#IN SAMPLE MODEL PERFORMANCE
#Predicted values for training data set ()
in_model_sv_score <- predict(model_sv, train_sv, probability=TRUE)
head(attr(in_model_sv_score, "probabilities")) 
in_model_sv_score <- attr(in_model_sv_score, "probabilities")[,1]

#Create ROC curve for training sample
in_roc_sv <- roc(response=train_sv$DEFAULT, predictor=in_model_sv_score)
print(in_roc_sv)

# Compute AUC
in_auc_sv <- auc(in_roc_sv)
in_auc_sv

#Find the threshold value recommended by the ROC curve
coords(roc=in_roc_sv,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

in_roc_sv.specs <- coords(roc=in_roc_sv,x=c('best'),
                          input=c('threshold','specificity','sensitivity'),
                          ret=c('threshold','specificity','sensitivity'),
                          as.list=TRUE)
in_roc_sv.specs

# Add predicted values to the train dataframe
train_sv$in_modelscores <- in_model_sv_score

#Use the threshold to assign the classes
train_sv$in_classes <- ifelse(train_sv$in_modelscores > in_roc_sv.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(train_sv$DEFAULT, train_sv$in_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Calculate accuracy, TPR, FPR
in_sv_acc <- mean(train_sv$in_classes==train_sv$DEFAULT)
in_sv_acc
in_sv_TPR <- in_roc_sv.specs$sensitivity
in_sv_TPR
in_sv_FPR <- 1-in_roc_sv.specs$specificity
in_sv_FPR

#######################################################################
# OUT oF SAMPLE MODEL PERFORMANCE

#Prediction porbabilities on the test set.
out_model_sv_score <- predict(model_sv, test_sv, probability=TRUE)
head(attr(out_model_sv_score, "probabilities")) 
out_model_sv_score <- attr(out_model_sv_score, "probabilities")[,1]

#Create ROC curve for test sample
out_roc_sv <- roc(response=test_sv$DEFAULT, predictor=out_model_sv_score)
print(out_roc_sv)
plot(out_roc_sv)

# Compute AUC
out_auc_sv <- auc(out_roc_sv)
out_auc_sv

#Find the threshold value recommended by the ROC curve
coords(roc=out_roc_sv,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

out_roc_sv.specs <- coords(roc=out_roc_sv,x=c('best'),
                           input=c('threshold','specificity','sensitivity'),
                           ret=c('threshold','specificity','sensitivity'),
                           as.list=TRUE)
out_roc_sv.specs

# Add predicted values to the test dataframe
test_sv$out_modelscores <- out_model_sv_score

#Use the threshold to assign the classes
test_sv$out_classes <- ifelse(test_sv$out_modelscores > out_roc_sv.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(test_sv$DEFAULT, test_sv$out_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Check confusion matrix set out correctly
out_sv_acc <- mean(test_sv$out_classes==test_sv$DEFAULT)
out_sv_TPR <- out_roc_sv.specs$sensitivity
out_sv_FPR <- 1- out_roc_sv.specs$specificity
out_sv_acc  
out_sv_TPR 
out_sv_FPR 

#Create ROC plots
par(mfrow=c(1,2))
plot(in_roc_sv, main = "In Sample ROC Curve AUC = 0.75", cex.main=0.9)
plot(out_roc_sv, main = "Out of Sample ROC Curve AUC = 0.68", cex.main=0.9)


#######################################################################
#SVM with Continous Variables

#######################################################################
library(e1071)
#Load dataframe with continous variables
#c_default <- readRDS(file = 'C:/Users/dobbin/Desktop/Northwestern/498 Capstone/Model/c_default.rds')
c_default <- readRDS(file = 'C:/Users/mkdob/OneDrive/Northwestern/498 Capstone/Model/c_default.rds')
train_svc <- subset(c_default, data.group ==1)
train_svc <- subset(train_svc, select = -c(data.group))
test_svc <- subset(c_default, data.group ==2)

# Train model as well as calculate time to train.
set.seed(1)
start.time <- Sys.time()

#Use grid search to find optimal parameters #Result cost =0.5
svmtc <- tune(svm, DEFAULT ~ ., 
             data=train_svc, probability=TRUE, kernel="radial", ranges=list(cost=c(1,2), gamma=c(0.5,1)))


#model_svc <- svm(DEFAULT ~ ., data = train_svc, kernel="radial", 
                 #cost = 1, gamma= 0.5, probability=TRUE)

end.time = Sys.time()
model_svc.taken <- end.time - start.time
model_svc.taken
model_svc <- svmtc$best.model
summary(model_svc)



#######################################################################
#IN SAMPLE MODEL PERFORMANCE
#Predicted values for training data set ()
in_model_svc_score <- predict(model_svc, train_svc, probability=TRUE)
head(attr(in_model_svc_score, "probabilities")) 
in_model_svc_score <- attr(in_model_svc_score, "probabilities")[,1]

#Create ROC curve for training sample
in_roc_svc <- roc(response=train_svc$DEFAULT, predictor=in_model_svc_score)
print(in_roc_svc)

# Compute AUC
in_auc_svc <- auc(in_roc_svc)
in_auc_svc

#Find the threshold value recommended by the ROC curve
coords(roc=in_roc_svc,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

in_roc_svc.specs <- coords(roc=in_roc_svc,x=c('best'),
                          input=c('threshold','specificity','sensitivity'),
                          ret=c('threshold','specificity','sensitivity'),
                          as.list=TRUE)
in_roc_svc.specs

# Add predicted values to the train dataframe
train_svc$in_modelscores <- in_model_svc_score

#Use the threshold to assign the classes
train_svc$in_classes <- ifelse(train_svc$in_modelscores > in_roc_svc.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(train_svc$DEFAULT, train_svc$in_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Calculate accuracy, TPR, FPR
in_svc_acc <- mean(train_svc$in_classes==train_svc$DEFAULT)
in_svc_acc
in_svc_TPR <- in_roc_svc.specs$sensitivity
in_svc_TPR
in_svc_FPR <- 1-in_roc_svc.specs$specificity
in_svc_FPR

#######################################################################
# OUT oF SAMPLE MODEL PERFORMANCE

#Prediction porbabilities on the test set.
out_model_svc_score <- predict(model_svc, test_svc, probability=TRUE)
out_model_svc_score <- attr(out_model_svc_score, "probabilities")[,1]

#Create ROC curve for test sample
out_roc_svc <- roc(response=test_svc$DEFAULT, predictor=out_model_svc_score)
print(out_roc_svc)
plot(out_roc_svc)

# Compute AUC
out_auc_svc <- auc(out_roc_svc)
out_auc_svc

#Find the threshold value recommended by the ROC curve
coords(roc=out_roc_svc,x=c('best'),
       input=c('threshold','specificity','sensitivity'),
       ret=c('threshold','specificity','sensitivity'),
       as.list=TRUE)

out_roc_svc.specs <- coords(roc=out_roc_svc,x=c('best'),
                           input=c('threshold','specificity','sensitivity'),
                           ret=c('threshold','specificity','sensitivity'),
                           as.list=TRUE)
out_roc_svc.specs

# Add predicted values to the test dataframe
test_svc$out_modelscores <- out_model_svc_score

#Use the threshold to assign the classes
test_svc$out_classes <- ifelse(test_svc$out_modelscores > out_roc_svc.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(test_svc$DEFAULT, test_svc$out_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Check confusion matrix set out correctly
out_svc_acc <- mean(test_svc$out_classes==test_svc$DEFAULT)
out_svc_TPR <- out_roc_svc.specs$sensitivity
out_svc_FPR <- 1- out_roc_svc.specs$specificity
out_svc_acc  
out_svc_TPR 
out_svc_FPR 

#Create ROC plots
par(mfrow=c(1,2))
plot(in_roc_svc, main = "In Sample ROC Curve AUC = 0.93", cex.main=0.9)
plot(out_roc_svc, main = "Out of Sample ROC Curve AUC = 0.71", cex.main=0.9)


###############################################################################
#Create a colourful combined plot of the test set ROC curves.
par(mfrow=c(1,1))
#AUC PLOT
plot(out_roc_rf)
plot(out_roc_lr, col=2, add=TRUE)
plot(out_roc_blr, col=3, add=TRUE)
plot(out_roc_gb, col=4, add=TRUE)
plot(out_roc_nb, col=5, add=TRUE)
plot(out_roc_sv, col=6, add=TRUE)
plot(out_roc_svc, col=7, add=TRUE)
#plot(out_roc_nn col=8, add=TRUE)

n1<-paste("Random Forest (AUC = ", round(out_auc_rf,2), ")", sep="")
n2<-paste("Logistic Regression (AUC = ", round(out_auc_lr,2), ")", sep="")
n3<-paste("Backward Logistic (AUC = ", round(out_auc_blr,2), ")", sep="")
n4<-paste("Gradient Boosted (AUC = ", round(out_auc_gb,2), ")", sep="")
n5<-paste("Naive Bayes (AUC = ", round(out_auc_nb,2), ")", sep="")
n6<-paste("SVM Discrete (AUC = ", round(out_auc_sv,2), ")", sep="")
n7<-paste("SVM Continous (AUC = ", round(out_auc_svc,2), ")", sep="")
#n8<-paste("Neural Network (AUC = ", round(out_auc_svc,3), ")", sep="")

legend("bottomright",c(n1,n2,n3,n4,n5,n6,n7),lty=1,col=1:7,inset=0.05)



