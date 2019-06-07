# 25.05.2019 
# Matthew Dobbin
# Northwestern University
# MSDS 498 Capstone Project

#########################################################################
# Production monitoring and validation of the Logistic Regression model 
# using the KS Statistic
#########################################################################

#Read in modeling dataframe.
#f_default <- readRDS(file = 'C:/Users/dobbin/Desktop/Northwestern/498 Capstone/Model/f_default.rds')
f_default <- readRDS(file = "C:/Users/mkdob/OneDrive/Northwestern/498 Capstone/Model/f_default.rds")


library(caret) #Required for confusion table normalization and gbm control
library(pROC) #Required for ROC curve
par(mfrow=c(1,2))

########################################################################
########################################################################

# Performance Monitoring Plan

########################################################################
########################################################################
#######################################################################
#
#LOGISTIC REGRESSION with backward selection
library(MASS)
train_blr <- subset(f_default, data.group ==1)
train_blr <- subset(train_blr, select = -c(data.group))
test_blr <- subset(f_default, data.group ==2)
valid_blr <- subset(f_default, data.group ==3)

#Issues with step AIC and DEFAULT being a factor. Change to numeric.
train_blr$DEFAULT <- as.numeric(train_blr$DEFAULT)-1
table(train_blr$DEFAULT)
test_blr$DEFAULT <- as.numeric(test_blr$DEFAULT)-1
table(test_blr$DEFAULT)
valid_blr$DEFAULT <- as.numeric(valid_blr$DEFAULT)-1
table(valid_blr$DEFAULT)

#Fitted a logistic regression model with backward variable selection
start.time <- Sys.time()
model_blr <- list()

# Define the upper model as the model with pool of influential variables from GBM 
lr <- glm(DEFAULT ~ PAY_R1_G1 + MAX_DLQ_G1  + PAY_R1_LE0  + AVG_PAY_AMT_LE2000  + 
                  PAY_R5_G0  + PAY_R3_G0 + PAY_R2_G0  + MAX_BILL_AMT_LE600  + BAL_GROWTH_6MO_N10172_923 +
                  MARRIAGE_MD  + MAX_BILL_AMT_600_4079  + UTIL_AUG_G24 + AVG_PMT_RATIO_46_1 + 
                  MAX_BILL_AMT_4079_18400  + SEX_M, data=train_blr, family='binomial') 
summary(lr) 

#Call stepAIC() for variable selection
model_blr <- lr
end.time = Sys.time()
model_blr.taken <- end.time - start.time
model_blr.taken


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
#Create components of a KS table for training set data
train_ks <- as.data.frame(cbind(in_model_blr_score,train_blr$DEFAULT));
head(train_ks)

#Semi Decile Model Scores
semi_decile_pts <- quantile(train_ks$in_model_blr_score,
                       probs=c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
                               0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8,
                               0.85, 0.9, 0.95));

train_ks$model_decile <- cut(train_ks$in_model_blr_score,breaks=c(0,semi_decile_pts,1),
                                labels=rev(c('01','02','03','04','05','06','07','08','09','10',
                                             '11','12','13','14','15','16','17','18','19','20')))
colnames(train_ks) <- c('model_score', 'response', 'model_decile')
head(train_ks)

#Check the min score in each model decile;
aggregate(train_ks$model_score,by=list(Decile=train_ks$model_decile),FUN=min);

table(train_ks$model_decile)

table(train_ks$model_decile, train_ks$response)

ks.table <- as.data.frame(list(Y0=table(train_ks$model_decile,train_ks$response)[,1],
                               Y1=table(train_ks$model_decile,train_ks$response)[,2],
                               Decile=rev(c('01','02','03','04','05','06','07','08','09','10',
                                            '11','12','13','14','15','16','17','18','19','20'))
));


# Sort the data frame by decile;
ks.table[order(ks.table$Decile),]


#######################################################################
# OUT oF SAMPLE MODEL PERFORMANCE

#Prediction probabilities on the test set.
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
test_blr$out_classes <- ifelse(test_blr$out_modelscores > in_roc_blr.specs$threshold, 1, 0)

#Create Confusion matrix
t <- table(test_blr$DEFAULT, test_blr$out_classes)
t
#Normalize confusion matrix into rates.
prop.table(t)

#Check confusion matrix set out correctly
out_blr_acc <- mean(test_blr$out_classes==test_blr$DEFAULT)
out_blr_TPR <- 0.13819473 / (0.13819473 + 0.07442305)
out_blr_FPR <- 0.18353134 / (0.18353134 + 0.60385088)
out_blr_acc  
out_blr_TPR 
out_blr_FPR 

#Create ROC plots
par(mfrow=c(1,2))
plot(in_roc_blr, main = "In Sample ROC Curve AUC = 0.78", cex.main=0.9)
plot(out_roc_blr, main = "Out of Sample ROC Curve AUC = 0.78", cex.main=0.9)

#######################################################################
#Create components of a KS table for test set data
test_ks <- as.data.frame(cbind(out_model_blr_score,test_blr$DEFAULT));
head(test_ks)

#Semi Decile Model Scores
semi_decile_pts <- quantile(test_ks$out_model_blr_score,
                            probs=c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
                                    0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8,
                                    0.85, 0.9, 0.95));

test_ks$model_decile <- cut(test_ks$out_model_blr_score,breaks=c(0,semi_decile_pts,1),
                             labels=rev(c('01','02','03','04','05','06','07','08','09','10',
                                          '11','12','13','14','15','16','17','18','19','20')))
colnames(test_ks) <- c('model_score', 'response', 'model_decile')
head(test_ks)

#Check the min score in each model decile;
aggregate(test_ks$model_score,by=list(Decile=test_ks$model_decile),FUN=min);

table(test_ks$model_decile)

table(test_ks$model_decile, test_ks$response)

ks.table <- as.data.frame(list(Y0=table(test_ks$model_decile,test_ks$response)[,1],
                               Y1=table(test_ks$model_decile,test_ks$response)[,2],
                               Decile=rev(c('01','02','03','04','05','06','07','08','09','10',
                                            '11','12','13','14','15','16','17','18','19','20'))
));


# Sort the data frame by decile;
ks.table[order(ks.table$Decile),]



#######################################################################
#Prediction probabilities on the validation set.
val_model_blr_score <- predict(model_blr, valid_blr, type = "response")

#Create components of a KS table for validation set data
valid_ks <- as.data.frame(cbind(val_model_blr_score,valid_blr$DEFAULT));
head(valid_ks)

#Semi Decile Model Scores
semi_decile_pts <- quantile(valid_ks$val_model_blr_score,
                            probs=c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
                                    0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8,
                                    0.85, 0.9, 0.95))

valid_ks$model_decile <- cut(valid_ks$val_model_blr_score,breaks=c(0,semi_decile_pts,1),
                            labels=rev(c('01','02','03','04','05','06','07','08','09','10',
                                         '11','12','13','14','15','16','17','18','19','20')))
colnames(valid_ks) <- c('model_score', 'response', 'model_decile')
head(valid_ks)

#Check the min score in each model decile;
aggregate(valid_ks$model_score,by=list(Decile=valid_ks$model_decile),FUN=min);

table(valid_ks$model_decile)

table(valid_ks$model_decile, valid_ks$response)

ks.table <- as.data.frame(list(Y0=table(valid_ks$model_decile,valid_ks$response)[,1],
                               Y1=table(valid_ks$model_decile,valid_ks$response)[,2],
                               Decile=rev(c('01','02','03','04','05','06','07','08','09','10',
                                            '11','12','13','14','15','16','17','18','19','20'))
));


# Sort the data frame by decile;
ks.table[order(ks.table$Decile),]


