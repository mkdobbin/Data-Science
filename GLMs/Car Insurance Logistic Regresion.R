#Predict 411 Assignment 2
#20/10/2017
#Auto Insurance Project

##################################
#Code is set up in two sections. First section involves the data cleaning and
#building predictive models. The second section is the stand alone test
#scoring program.
##################################

#Install packages
library(corrplot)
library(lessR)
require(moments)
require(ggplot2)
require(gridExtra)
require(reshape2)
require(rockchalk)
require(flux)
require(car)
library(MASS)
library(Hmisc)
library(pscl)
library(pROC)
library(ROCR)

# Read in csv file for moneyball data;
path.name <- 'C:/Users/mkdob/OneDrive/Northwestern/411 GLM/Assignment 2/';
file.name <- paste(path.name,'logit_insurance.csv', sep='');

# Read in the csv file into an R data frame;
mydata <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);
colnames(mydata)[1] <- "INDEX"
mydata2 <- cbind.data.frame(mydata$INDEX)
str(mydata)

#Explore data using Summarystats
SummaryStats(TARGET_AMT, data = mydata)
SummaryStats(AGE, data = mydata)
SummaryStats(BLUEBOOK, data = mydata)
SummaryStats(CAR_AGE, data = mydata)
SummaryStats(CAR_TYPE, data = mydata)
SummaryStats(CAR_USE, data = mydata)
SummaryStats(CLM_FREQ, data = mydata)
SummaryStats(EDUCATION, data = mydata)
SummaryStats(HOMEKIDS, data = mydata)
SummaryStats(HOME_VAL, data = mydata)
SummaryStats(INCOME, data = mydata)
SummaryStats(JOB, data = mydata)
SummaryStats(KIDSDRIV, data = mydata)
SummaryStats(MSTATUS, data = mydata)
SummaryStats(MVR_PTS, data = mydata)
SummaryStats(OLDCLAIM, data = mydata)
SummaryStats(PARENT1, data = mydata)
SummaryStats(RED_CAR, data = mydata)
SummaryStats(REVOKED, data = mydata)
SummaryStats(REVOKED, data = mydata)
SummaryStats(SEX, data = mydata)
SummaryStats(TIF, data = mydata)
SummaryStats(TRAVTIME, data = mydata)
SummaryStats(URBANICITY, data = mydata)
SummaryStats(YOJ, data = mydata)
boxplot(mydata$TRAVTIME)
Histogram(TARGET_AMT, data=mydata)

###AGE#######
#Remove NAs and Impute Mean
MAGE <- mean(mydata$AGE, na.rm=TRUE)
mydata$M_AGE <- ifelse(is.na(mydata$AGE), 1, 0)
mydata$IMP_AGE <- ifelse(is.na(mydata$AGE), MAGE, mydata$AGE)
mydata$AGE_L <- ifelse(mydata$IMP_AGE <= 25, 1,0)
mydata$AGE_H <- ifelse(mydata$IMP_AGE > 65, 1,0)

#### CAR_AGE
#Remove negative values, NAs and impute mean.
mydata$CAR_AGE<-ifelse(mydata$CAR_AGE <0, NA,mydata$CAR_AGE)
MCAR_AGE <- mean(mydata$CAR_AGE, na.rm=TRUE)
mydata$M_CAR_AGE <- ifelse(is.na(mydata$CAR_AGE), 1, 0)
mydata$IMP_CAR_AGE <- ifelse(is.na(mydata$CAR_AGE), MCAR_AGE, mydata$CAR_AGE)

#### HOME_VAL
MVAL <- mean(mydata$HOME_VAL, na.rm=TRUE)
mydata$M_HOME_VAL <- ifelse(is.na(mydata$HOME_VAL), 1, 0)
mydata$IMP_HOME_VAL <- ifelse(is.na(mydata$HOME_VAL), MVAL, mydata$HOME_VAL)

#### INCOME
MINC <- mean(mydata$INCOME, na.rm=TRUE)
mydata$M_INCOME <- ifelse(is.na(mydata$INCOME), 1, 0)
mydata$IMP_INCOME<- ifelse(is.na(mydata$INCOME), MINC, mydata$INCOME)

#### YOJ
MYOJ <- mean(mydata$YOJ, na.rm=TRUE)
mydata$M_YOJ <- ifelse(is.na(mydata$YOJ), 1, 0)
mydata$IMP_YOJ<- ifelse(is.na(mydata$YOJ), MYOJ, mydata$YOJ)

#### CAR_TYPE
mydata$CT_PICKUP <- ifelse(mydata$CAR_TYPE =="Pickup", 1,0)
mydata$CT_MVAN <- ifelse(mydata$CAR_TYPE =="Minivan", 1,0)
mydata$CT_SPORTS <- ifelse(mydata$CAR_TYPE =="Sports Car", 1,0)
mydata$CT_VAN <- ifelse(mydata$CAR_TYPE =="Van", 1,0)
mydata$CT_SUV <- ifelse(mydata$CAR_TYPE =="z_SUV", 1,0)

#### CAR_USE
mydata$CU_COMMERCIAL <- ifelse(mydata$CAR_USE == "Commercial", 1, 0) 

#### EDUCATION
mydata$ED_NOHS <- ifelse(mydata$EDUCATION =="<High School", 1,0)
mydata$ED_BACH <- ifelse(mydata$EDUCATION =="Bachelors", 1,0)
mydata$ED_MAST <- ifelse(mydata$EDUCATION =="Masters", 1,0)
mydata$ED_PHD <- ifelse(mydata$EDUCATION =="PhD", 1,0)

#### JOB
mydata$JOB_CLERK <- ifelse(mydata$JOB == "Clerical", 1, 0)
mydata$JOB_DOC <- ifelse(mydata$JOB == "Doctor", 1, 0)
mydata$JOB_HOME <- ifelse(mydata$JOB == "Home Maker", 1, 0)
mydata$JOB_LAW <- ifelse(mydata$JOB == "Lawyer", 1, 0)
mydata$JOB_MAN <- ifelse(mydata$JOB == "Manager", 1, 0)
mydata$JOB_PROF <- ifelse(mydata$JOB == "Professional", 1, 0)
mydata$JOB_STU <- ifelse(mydata$JOB == "Student", 1, 0)
mydata$JOB_BLUE <- ifelse(mydata$JOB == "z_Blue Collar", 1, 0)

#### MSTATUS
mydata$MS_YES <- ifelse(mydata$MSTATUS == "Yes", 1, 0)

#### PARENT1
mydata$PA_1 <- ifelse(mydata$PARENT1 == "Yes", 1, 0)

#### RED_CAR
mydata$RED_YES <- ifelse(mydata$RED_CAR == "yes", 1, 0)

#### REVOKED
mydata$RVOK_YES <- ifelse(mydata$REVOKED == "Yes", 1, 0)

#### SEX
mydata$SEX_M <- ifelse(mydata$SEX == "M", 1, 0)

#### URBANICITY
mydata$RURAL <- ifelse(mydata$URBANICITY== "z_Highly Rural/ Rural", 1, 0)

##################
#Create Severity Model by building a regression for the target variable TARGET_AMT when the car
#has had a crash. (Amounts greater than 0).
###################
target_amt <- mydata$TARGET_AMT
bluebook <- mydata$BLUEBOOK
car_age <- mydata$IMP_CAR_AGE
y_amount <-ifelse(target_amt> 0 , target_amt, NA)
mydata2 <- cbind.data.frame(mydata2, target_amt, bluebook, car_age, y_amount)
SummaryStats(y_amount, data = mydata2)
SummaryStats(TARGET_AMT, data = mydata)
sev_fit<- reg(y_amount ~ bluebook+ car_age , data = mydata2)  
sev_fit
p_target_amount <- 4435.9299 + 0.116327*bluebook-52.728785*car_age

#### Update mydata dataframe with the variables to be used in the models
#### AGE_L & AGE_H will be used only in Model 3
keep.var<- c("TARGET_FLAG",	"CT_VAN",	"M_INCOME", "IMP_INCOME", "OLDCLAIM",
              "CT_SUV",	"JOB_CLERK",	"PA_1",
              "M_AGE",	"CU_COMMERCIAL",	"JOB_DOC",	"RED_YES",
              "IMP_AGE",	"CLM_FREQ",	"JOB_HOME",	"RVOK_YES",
              "ED_NOHS",	"JOB_LAW",	"SEX_M", "ED_BACH",	"JOB_MAN",
              "TIF", "BLUEBOOK", "ED_MAST", "JOB_PROF",
              "TRAVTIME", "IMP_CAR_AGE",	"ED_PHD", "JOB_STU",
              "RURAL", "M_CAR_AGE", "HOMEKIDS", "JOB_BLUE",
              "IMP_YOJ", "CT_MVAN", "IMP_HOME_VAL", "KIDSDRIV",
              "M_YOJ", "CT_PICKUP" , "M_HOME_VAL", "MS_YES")
mydata <- mydata[, keep.var]              
mydata.cor <- cor(mydata)
par(mar = c(2,2,6.1,1))
corrplot(mydata.cor, method = "square", tl.cex=0.75, tl.col = "black")

#################
#Regression Model Building
#################
#Model 1
M1_lR <- Logit(TARGET_FLAG ~ OLDCLAIM + PA_1 + CU_COMMERCIAL +
              CLM_FREQ + RVOK_YES + JOB_BLUE + IMP_INCOME +
              RURAL + MS_YES + IMP_HOME_VAL, data = mydata)

M1_glm <- glm(TARGET_FLAG ~ OLDCLAIM + PA_1 + CU_COMMERCIAL +
              CLM_FREQ + RVOK_YES + JOB_BLUE + IMP_INCOME +
              RURAL + MS_YES + IMP_HOME_VAL,
              family = binomial(link = "logit"), data=mydata)
M_null <- glm(TARGET_FLAG ~ 1,
              family = binomial(link = "logit"), data=mydata)
summary(M1_glm)
pR2(M1_glm)

# Calculate metrics
logLik(M1_glm)
logLik(M_null)
k <- 10
llnull <- -4708.981
llunc <- -3921.933
R2MCF <- 1-(((llunc)-(k+1))/(llnull))
Du = -2*llunc
Du
AIC <- (Du + 2*(k+1))
AIC

#Model 1 Hardcoded
y1_pred1 <- -0.6277857177-0.0000120929*mydata$OLDCLAIM+0.6747496815*mydata$PA_1+
            0.6772250204*mydata$CU_COMMERCIAL+0.2898930114*mydata$CLM_FREQ+
            0.9154599432*mydata$RVOK_YES+0.3179920279*mydata$JOB_BLUE+
            -0.0000091604*mydata$IMP_INCOME-2.0345857885*mydata$RURAL+
            -0.2532439673*mydata$MS_YES-0.0000017435*mydata$IMP_HOME_VAL 
y1_pred2 <- exp(y1_pred1)
p1_logistic <- y1_pred2/(1+y1_pred2)
p1_logistic
#Create ROC Curve
pred<- prediction(p1_logistic, mydata$TARGET_FLAG)
perf <- performance(pred, "tpr", "fpr")
par(mar=c(5,6,4,2)+0.1)
plot(perf, type = "l", col = "blue",
     main = "Model 1 ROC Curve, KS = 0.415, AUC = 0.77", 
     xlab = "False positive rate", ylab = "True positive rate", lwd=3)
abline(a=0, b=1, col = "red", lty = 2, lwd = 2)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)
KS1 <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
KS1

##########
#Model 2 Backward Variable Selection Model
backward.lm <- list()
upper.lm <- glm(TARGET_FLAG ~ ., family = binomial(link = "logit"), data=mydata); 
backward.lm <- stepAIC(object=upper.lm,direction=c('backward')); 
summary(backward.lm)
pR2(backward.lm)
# Calculate metrics
logLik(backward.lm)
k <- 27
llnull <- -4708.981
llunc <- -3685.441
R2MCF <- 1-(((llunc)-(k+1))/(llnull))
R2MCF
Du = -2*llunc
Du
AIC <- (Du + 2*(k+1))
AIC

#Backward model hardcoded
yB_pred1 <- 0.086936216-0.000003865*mydata$IMP_INCOME-0.000011208*mydata$OLDCLAIM+
          -0.151391408*mydata$CT_SUV+0.233502980*mydata$JOB_CLERK+0.397360863*mydata$PA_1+
          2.251071469*mydata$M_AGE+0.715492200*mydata$CU_COMMERCIAL-0.511644384*mydata$JOB_DOC+
          0.260075777*mydata$CLM_FREQ+0.861631766*mydata$RVOK_YES-0.104333665*mydata$SEX_M+
          -0.383509481*mydata$ED_BACH-0.717960565*mydata$JOB_MAN-0.057347270*mydata$TIF+
          -0.000028174*mydata$BLUEBOOK-0.367272376*mydata$ED_MAST+0.014844455*mydata$TRAVTIME+
          -0.265617253*mydata$ED_PHD-2.429673109*mydata$RURAL+0.054617319*mydata$HOMEKIDS+
          0.169077740*mydata$JOB_BLUE-0.013775282*mydata$IMP_YOJ-0.796854425*mydata$CT_MVAN+
          -0.000001382*mydata$IMP_HOME_VAL+0.397251010*mydata$KIDSDRIV-0.224643055*mydata$CT_PICKUP+
          -0.480644713*mydata$MS_YES       
yB_pred2 <- exp(yB_pred1)
pB_logistic <- yB_pred2/(1+yB_pred2)
pB_logistic

#Create ROC Curve
pred<- prediction(pB_logistic, mydata$TARGET_FLAG)
perf <- performance(pred, "tpr", "fpr")
par(mar=c(5,6,4,2)+0.1)
plot(perf, type = "l", col = "blue",
     main = "Model 2 ROC Curve, KS = 0.47, AUC = 0.81", 
     xlab = "False positive rate", ylab = "True positive rate", lwd=3)
abline(a=0, b=1, col = "red", lty = 2, lwd = 2)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)
KS2 <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
KS2

##########
#Model 3 Replace AGE with bins and use backward
#Put age into groups AGE_L & AGE_H
mydata$AGE_L <- ifelse(mydata$IMP_AGE <= 25, 1,0)
mydata$AGE_H <- ifelse(mydata$IMP_AGE > 65, 1,0)
keep.var<- c("TARGET_FLAG",	"CT_VAN",	"M_INCOME", "IMP_INCOME", "OLDCLAIM",
              "CT_SUV",	"JOB_CLERK",	"PA_1",
              "AGE_L",	"CU_COMMERCIAL",	"JOB_DOC",	"RED_YES",
              "AGE_H",	"CLM_FREQ",	"JOB_HOME",	"RVOK_YES",
              "ED_NOHS",	"JOB_LAW",	"SEX_M", "ED_BACH",	"JOB_MAN",
              "TIF", "BLUEBOOK", "ED_MAST", "JOB_PROF",
              "TRAVTIME", "IMP_CAR_AGE",	"ED_PHD", "JOB_STU",
              "RURAL", "M_CAR_AGE", "HOMEKIDS", "JOB_BLUE",
              "IMP_YOJ", "CT_MVAN", "IMP_HOME_VAL", "KIDSDRIV",
              "M_YOJ", "CT_PICKUP" , "M_HOME_VAL", "MS_YES")

mydata <- mydata[, keep.var] 
backward.lm2 <- list()
upper.lm2 <- glm(TARGET_FLAG ~ ., family = binomial(link = "logit"), data=mydata); 
backward.lm2 <- stepAIC(object=upper.lm2,direction=c('backward')); 
summary(backward.lm2)
pR2(backward.lm2)
logLik(backward.lm2)
k <- 24
llnull <- -4708.981
llunc <- -3680.417
R2MCF <- 1-(((llunc)-(k+1))/(llnull))
R2MCF
Du = -2*llunc
Du
AIC <- (Du + 2*(k+1))
AIC

#Model 3 hard coded
yB2_pred1 <- -0.0803885926-0.0000043505*mydata$IMP_INCOME-0.0000109999*mydata$OLDCLAIM+
              0.2594073624*mydata$JOB_CLERK+0.3763134543*mydata$PA_1+1.0946490791*mydata$AGE_L+         
              0.6952036363*mydata$CU_COMMERCIAL-0.6883424477*mydata$JOB_DOC+       
              0.2585946715*mydata$CLM_FREQ+0.8568861700*mydata$RVOK_YES+      
              -0.3419352446*mydata$ED_BACH-0.7191106228*mydata$JOB_MAN+       
              -0.0578797334*mydata$TIF-0.0000262141*mydata$BLUEBOOK-0.2886862957*mydata$ED_MAST+       
              0.0147496273*mydata$TRAVTIME-2.4211769557*mydata$RURAL+0.0511172454*mydata$HOMEKIDS+      
              0.2302712468*mydata$JOB_BLUE-0.0140239496*mydata$IMP_YOJ-0.7712495231*mydata$CT_MVAN+       
              -0.0000013913*mydata$IMP_HOME_VAL+0.4153405129*mydata$KIDSDRIV+      
              -0.1823802266*mydata$CT_PICKUP-0.4735555009*mydata$MS_YES     
yB2_pred2 <- exp(yB2_pred1)
pB2_logistic <- yB2_pred2/(1+yB2_pred2)

#Create ROC Curve
pred<- prediction(pB2_logistic, mydata$TARGET_FLAG)
perf <- performance(pred, "tpr", "fpr")
par(mar=c(5,6,4,2)+0.1)
plot(perf, type = "l", col = "blue",
     main = "Model 3 ROC Curve, KS = 0.48 , AUC = 0.81", 
     xlab = "False positive rate", ylab = "True positive rate", lwd=3)
abline(a=0, b=1, col = "red", lty = 2, lwd = 2)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)
KS3 <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
KS3







##################################################################
##################################################################
########   Section 2: Scoring Program using Model 3
##################################################################
##################################################################

#Install packages
library(corrplot)
library(lessR)
require(moments)
require(ggplot2)
require(gridExtra)
require(reshape2)
require(rockchalk)
require(flux)
require(car)
library(MASS)
library(Hmisc)
library(pscl)
library(pROC)

#Read in insurance test dataset. File path will require changing to 
#where you have stored the file on your computer.
path.name <- 'C:/Users/mkdob/OneDrive/Northwestern/411 GLM/Assignment 2/';
file.name <- paste(path.name,'logit_insurance_test.csv', sep='');
mydata <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);
colnames(mydata)[1] <- "INDEX"

######Data Preparation
###AGE#######
#Remove NAs and Impute Mean
MAGE <- mean(mydata$AGE, na.rm=TRUE)
mydata$M_AGE <- ifelse(is.na(mydata$AGE), 1, 0)
mydata$IMP_AGE <- ifelse(is.na(mydata$AGE), MAGE, mydata$AGE)
mydata$AGE_L <- ifelse(mydata$IMP_AGE <= 25, 1,0)
mydata$AGE_H <- ifelse(mydata$IMP_AGE > 65, 1,0)

#### CAR_AGE
#Remove negative values, NAs and impute mean.
mydata$CAR_AGE<-ifelse(mydata$CAR_AGE <0, NA,mydata$CAR_AGE)
MCAR_AGE <- mean(mydata$CAR_AGE, na.rm=TRUE)
mydata$M_CAR_AGE <- ifelse(is.na(mydata$CAR_AGE), 1, 0)
mydata$IMP_CAR_AGE <- ifelse(is.na(mydata$CAR_AGE), MCAR_AGE, mydata$CAR_AGE)

#### HOME_VAL
MVAL <- mean(mydata$HOME_VAL, na.rm=TRUE)
mydata$M_HOME_VAL <- ifelse(is.na(mydata$HOME_VAL), 1, 0)
mydata$IMP_HOME_VAL <- ifelse(is.na(mydata$HOME_VAL), MVAL, mydata$HOME_VAL)

#### INCOME
MINC <- mean(mydata$INCOME, na.rm=TRUE)
mydata$M_INCOME <- ifelse(is.na(mydata$INCOME), 1, 0)
mydata$IMP_INCOME<- ifelse(is.na(mydata$INCOME), MINC, mydata$INCOME)

#### YOJ
MYOJ <- mean(mydata$YOJ, na.rm=TRUE)
mydata$M_YOJ <- ifelse(is.na(mydata$YOJ), 1, 0)
mydata$IMP_YOJ<- ifelse(is.na(mydata$YOJ), MYOJ, mydata$YOJ)

#### CAR_TYPE
mydata$CT_PICKUP <- ifelse(mydata$CAR_TYPE =="Pickup", 1,0)
mydata$CT_MVAN <- ifelse(mydata$CAR_TYPE =="Minivan", 1,0)
mydata$CT_SPORTS <- ifelse(mydata$CAR_TYPE =="Sports Car", 1,0)
mydata$CT_VAN <- ifelse(mydata$CAR_TYPE =="Van", 1,0)
mydata$CT_SUV <- ifelse(mydata$CAR_TYPE =="z_SUV", 1,0)

#### CAR_USE
mydata$CU_COMMERCIAL <- ifelse(mydata$CAR_USE == "Commercial", 1, 0) 

#### EDUCATION
mydata$ED_NOHS <- ifelse(mydata$EDUCATION =="<High School", 1,0)
mydata$ED_BACH <- ifelse(mydata$EDUCATION =="Bachelors", 1,0)
mydata$ED_MAST <- ifelse(mydata$EDUCATION =="Masters", 1,0)
mydata$ED_PHD <- ifelse(mydata$EDUCATION =="PhD", 1,0)

#### JOB
mydata$JOB_CLERK <- ifelse(mydata$JOB == "Clerical", 1, 0)
mydata$JOB_DOC <- ifelse(mydata$JOB == "Doctor", 1, 0)
mydata$JOB_HOME <- ifelse(mydata$JOB == "Home Maker", 1, 0)
mydata$JOB_LAW <- ifelse(mydata$JOB == "Lawyer", 1, 0)
mydata$JOB_MAN <- ifelse(mydata$JOB == "Manager", 1, 0)
mydata$JOB_PROF <- ifelse(mydata$JOB == "Professional", 1, 0)
mydata$JOB_STU <- ifelse(mydata$JOB == "Student", 1, 0)
mydata$JOB_BLUE <- ifelse(mydata$JOB == "z_Blue Collar", 1, 0)

#### MSTATUS
mydata$MS_YES <- ifelse(mydata$MSTATUS == "Yes", 1, 0)

#### PARENT1
mydata$PA_1 <- ifelse(mydata$PARENT1 == "Yes", 1, 0)

#### RED_CAR
mydata$RED_YES <- ifelse(mydata$RED_CAR == "yes", 1, 0)

#### REVOKED
mydata$RVOK_YES <- ifelse(mydata$REVOKED == "Yes", 1, 0)

#### SEX
mydata$SEX_M <- ifelse(mydata$SEX == "M", 1, 0)

#### URBANICITY
mydata$RURAL <- ifelse(mydata$URBANICITY== "z_Highly Rural/ Rural", 1, 0)

################
#Logistic regression model
yB2_pred1 <- -0.0803885926-0.0000043505*mydata$IMP_INCOME-0.0000109999*mydata$OLDCLAIM+
              0.2594073624*mydata$JOB_CLERK+0.3763134543*mydata$PA_1+1.0946490791*mydata$AGE_L+         
              0.6952036363*mydata$CU_COMMERCIAL-0.6883424477*mydata$JOB_DOC+       
              0.2585946715*mydata$CLM_FREQ+0.8568861700*mydata$RVOK_YES+      
              -0.3419352446*mydata$ED_BACH-0.7191106228*mydata$JOB_MAN+       
              -0.0578797334*mydata$TIF-0.0000262141*mydata$BLUEBOOK-0.2886862957*mydata$ED_MAST+       
              0.0147496273*mydata$TRAVTIME-2.4211769557*mydata$RURAL+0.0511172454*mydata$HOMEKIDS+      
              0.2302712468*mydata$JOB_BLUE-0.0140239496*mydata$IMP_YOJ-0.7712495231*mydata$CT_MVAN+       
              -0.0000013913*mydata$IMP_HOME_VAL+0.4153405129*mydata$KIDSDRIV+      
              -0.1823802266*mydata$CT_PICKUP-0.4735555009*mydata$MS_YES     
yB2_pred2 <- exp(yB2_pred1)
p_target_flag <- yB2_pred2/(1+yB2_pred2)
p_target_flag

#Create data frame with results
index <- mydata$INDEX
mydata2 <- cbind.data.frame(index, p_target_flag)

#Create Severity Model by building a regression for the target variable TARGET_AMT when the car
#has had a crash. (Amounts greater than 0).
bluebook <- mydata$BLUEBOOK
car_age <- mydata$IMP_CAR_AGE
p_target_amount <- 4435.9299 + 0.116327*bluebook-52.728785*car_age
mydata3 <- cbind.data.frame(mydata2, p_target_amount)

#Output mydata3 dataframe into CSV file.
path.name <- 'C:/Users/mkdob/OneDrive/Northwestern/411 GLM/Assignment 2/';
file.name <- paste(path.name,'Dobbin_Predicted.csv', sep='');
write.csv(mydata3, file = file.name)

















































