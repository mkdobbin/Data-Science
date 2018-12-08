#Predict 411 Assignment 3
#08/11/2017
#Wine Tasting

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
path.name <- 'C:/Users/mkdob/OneDrive/Northwestern/411 GLM/Assignment 3/';
file.name <- paste(path.name,'wine.csv', sep='');

# Read in the csv file into an R data frame;
mydata <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);
colnames(mydata)[1] <- "INDEX"
str(mydata)

#######################################
#EDA
#######################################

#Summary stats
SummaryStats(TARGET, data = mydata)
SummaryStats(AcidIndex, data = mydata)
SummaryStats(Alcohol, data = mydata)
SummaryStats(Chlorides, data = mydata)
SummaryStats(CitricAcid, data = mydata)
SummaryStats(Density, data = mydata)
SummaryStats(FixedAcidity, data = mydata)
SummaryStats(FreeSulfurDioxide, data = mydata)
SummaryStats(LabelAppeal, data = mydata)
SummaryStats(ResidualSugar, data = mydata)
SummaryStats(STARS, data = mydata)
SummaryStats(Sulphates, data = mydata)
SummaryStats(TotalSulfurDioxide, data = mydata)
SummaryStats(VolatileAcidity, data = mydata)
SummaryStats(pH, data = mydata)

#Target Variable
hist(mydata$TARGET, col = "blue", main = "Histogram of TARGET", xlab = "Sample Cases")

#Acid Index
par(mfrow = c(1, 2))
hist(mydata$AcidIndex, col = "blue", xlab = "Acid Index")
boxplot(mydata$AcidIndex, ylab = "Acid Index")

#Alcohol
hist(mydata$Alcohol, col = "blue", xlab = "Alcohol")
boxplot(mydata$Alcohol, ylab = "Alcohol")

#Chlorides
hist(mydata$Chlorides, col = "blue", xlab = "Chlorides")
boxplot(mydata$Chlorides, ylab = "Chlorides")

#Citric Acid
hist(mydata$CitricAcid, col = "blue", xlab = "Citric Acid")
boxplot(mydata$CitricAcid, ylab = "Citric Acid")

#Density
hist(mydata$Density, col = "blue", xlab = "Density")
boxplot(mydata$Density, ylab = "Density")

#Fixed Acidity
hist(mydata$FixedAcidity, col = "blue", xlab = "Fixed Acidity")
boxplot(mydata$FixedAcidity, ylab = "Fixed Acidity")

#Free Sulfur Dioxide
hist(mydata$FreeSulfurDioxide, col = "blue", xlab = "Free Sulfur Dioxide")
boxplot(mydata$FreeSulfurDioxide, ylab = "Free Sulfur Dioxide")

#Label Appeal
hist(mydata$LabelAppeal, col = "blue", xlab = "LabelAppeal")
boxplot(mydata$TARGET ~ mydata$LabelAppeal, las=2, ylab = "Sample Cases", xlab = "Label Appeal")

#Residual Sugar
hist(mydata$ResidualSugar, col = "blue", xlab = "Residual Sugar")
boxplot(mydata$ResidualSugar, ylab = "Residual Sugar")

#STARS
hist(mydata$STARS, col = "blue", xlab = "STARS")
boxplot(mydata$TARGET ~ mydata$STARS, las=2, ylab = "Sample Cases", xlab = "STARS")

#Sulphates
hist(mydata$Sulphates, col = "blue", xlab = "Sulphates")
boxplot(mydata$Sulphates, ylab = "Sulphates")

#TotalSulfurDioxide
hist(mydata$TotalSulfurDioxide, col = "blue", xlab = "TotalSulfurDioxide")
boxplot(mydata$TotalSulfurDioxide, ylab = "TotalSulfurDioxide")

#VolatileAcidity
hist(mydata$VolatileAcidity, col = "blue", xlab = "VolatileAcidity")
boxplot(mydata$VolatileAcidity, ylab = "VolatileAcidity")

#pH
hist(mydata$pH, col = "blue", xlab = "pH")
boxplot(mydata$pH, ylab = "pH")


#############################
#Data Prep
#############################
#Acid Index - max cutoff, replace with mean
mydata$AcidIndex <-ifelse(mydata$AcidIndex > 14 ,NA, mydata$AcidIndex)
MEEN <- mean(mydata$AcidIndex, na.rm=TRUE)
mydata$M_AcidIndex <- ifelse(is.na(mydata$AcidIndex), 1, 0)
mydata$IMP_AcidIndex <- ifelse(is.na(mydata$AcidIndex), MEEN, mydata$AcidIndex)

#Alcohol - Use absolute values for negative values. Use mean for missing values. 
mydata$M_Alcohol <- ifelse(is.na(mydata$Alcohol),  1, ifelse(mydata$Alcohol < 0, 1, 0))
mydata$IMP_Alcohol <- abs(mydata$Alcohol)
MEEN <- mean(mydata$IMP_Alcohol, na.rm=TRUE)
mydata$IMP_Alcohol <- ifelse(is.na(mydata$IMP_Alcohol), MEEN, mydata$IMP_Alcohol)

#Chlorides - Use absolute values for negative values. Use mean missing values. 
mydata$M_Chlorides <- ifelse(is.na(mydata$Chlorides),  1, ifelse(mydata$Chlorides < 0, 1, 0))
mydata$IMP_Chlorides <- abs(mydata$Chlorides)
MEEN <- mean(mydata$IMP_Chlorides, na.rm=TRUE)
mydata$IMP_Chlorides <- ifelse(is.na(mydata$IMP_Chlorides), MEEN, mydata$IMP_Chlorides)

#CitricAcid - Use absolute values for negative values.
mydata$M_CitricAcid <- ifelse(is.na(mydata$CitricAcid),  1, ifelse(mydata$CitricAcid < 0, 1, 0))
mydata$IMP_CitricAcid <- abs(mydata$CitricAcid)

#Density - No treatment required

#FixedAcidity - Use absolute values for negative values.
mydata$M_FixedAcidity <- ifelse(is.na(mydata$FixedAcidity),  1, ifelse(mydata$FixedAcidity < 0, 1, 0))
mydata$IMP_FixedAcidity <- abs(mydata$FixedAcidity)

#FreeSulfurDioxide - Use absolute values for negative values. Use mean missing values. Maximum cutoff 500.
mydata$M_FreeSulfurDioxide <- ifelse(is.na(mydata$FreeSulfurDioxide), 1, 
                              ifelse(mydata$FreeSulfurDioxide < 0, 1, 
                              ifelse(mydata$FreeSulfurDioxide > 500, 1, 0)))
mydata$FreeSulfurDioxide <- abs(mydata$FreeSulfurDioxide)
mydata$FreeSulfurDioxide <-ifelse(mydata$FreeSulfurDioxide > 500 ,NA, mydata$FreeSulfurDioxide)
MEEN <- mean(mydata$FreeSulfurDioxide, na.rm=TRUE)
mydata$IMP_FreeSulfurDioxide <- ifelse(is.na(mydata$FreeSulfurDioxide), MEEN, mydata$FreeSulfurDioxide)

#LabelAppeal - No treatment required

#ResidualSugar - Use absolute values for negative values. Use mean missing values. 
mydata$M_ResidualSugar <- ifelse(is.na(mydata$ResidualSugar),  1, ifelse(mydata$ResidualSugar < 0, 1, 0))
mydata$IMP_ResidualSugar <- abs(mydata$ResidualSugar)
MEEN <- mean(mydata$IMP_ResidualSugar, na.rm=TRUE)
mydata$IMP_ResidualSugar <- ifelse(is.na(mydata$IMP_ResidualSugar), MEEN, mydata$IMP_ResidualSugar)

#STARS - Use mean missing values
mydata$M_STARS <- ifelse(is.na(mydata$STARS),  1, 0)
MEEN <- mean(mydata$STARS, na.rm=TRUE)
mydata$IMP_STARS <- ifelse(is.na(mydata$STARS), MEEN, mydata$STARS)

#Sulphates -Use absolute values for negative values. Use mean missing values. 
mydata$M_Sulphates <- ifelse(is.na(mydata$Sulphates),  1, ifelse(mydata$Sulphates < 0, 1, 0))
mydata$IMP_Sulphates <- abs(mydata$Sulphates)
MEEN <- mean(mydata$IMP_Sulphates, na.rm=TRUE)
mydata$IMP_Sulphates <- ifelse(is.na(mydata$IMP_Sulphates), MEEN, mydata$IMP_Sulphates)

#TotalSulfurDioxide - Use absolute values for negative values. Use mean missing values. Maximum cutoff 500.
mydata$M_TotalSulfurDioxide <- ifelse(is.na(mydata$TotalSulfurDioxide), 1, 
                                     ifelse(mydata$TotalSulfurDioxide < 0, 1, 
                                            ifelse(mydata$TotalSulfurDioxide > 500, 1, 0)))
mydata$TotalSulfurDioxide <- abs(mydata$TotalSulfurDioxide)
mydata$TotalSulfurDioxide <-ifelse(mydata$TotalSulfurDioxide > 500 ,NA, mydata$TotalSulfurDioxide)
MEEN <- mean(mydata$TotalSulfurDioxide, na.rm=TRUE)
mydata$IMP_TotalSulfurDioxide <- ifelse(is.na(mydata$TotalSulfurDioxide), MEEN, mydata$TotalSulfurDioxide)

#VolatileAcidity - Use absolute values for negative values.
mydata$M_VolatileAcidity <- ifelse(is.na(mydata$VolatileAcidity),  1, ifelse(mydata$VolatileAcidity < 0, 1, 0))
mydata$IMP_VolatileAcidity <- abs(mydata$VolatileAcidity)

#pH - Use mean missing values. Minimum cutoff 2,  Maximum cutoff 5.
mydata$M_pH <- ifelse(is.na(mydata$pH), 1, ifelse(mydata$pH < 2, 1, ifelse(mydata$pH > 5, 1, 0)))
mydata$pH <-ifelse(mydata$pH < 2 ,NA, mydata$pH)
mydata$pH <-ifelse(mydata$pH > 5 ,NA, mydata$pH)
MEEN <- mean(mydata$pH, na.rm=TRUE)
mydata$IMP_pH <- ifelse(is.na(mydata$pH), MEEN, mydata$pH)

# Update mydata dataframe with the variables to be used in the models
keep.var<- c("TARGET","IMP_AcidIndex","IMP_CitricAcid", "IMP_FreeSulfurDioxide", "IMP_Sulphates",
             "M_AcidIndex", "M_CitricAcid", "M_FreeSulfurDioxide", "M_Sulphates",
             "IMP_Alcohol", "Density", "IMP_ResidualSugar", "IMP_TotalSulfurDioxide",
             "M_Alcohol", "LabelAppeal", "M_ResidualSugar", "M_TotalSulfurDioxide",
             "IMP_Chlorides", "IMP_FixedAcidity", "IMP_STARS", "IMP_VolatileAcidity",
             "M_Chlorides", "M_FixedAcidity", "M_STARS",	"M_VolatileAcidity",
             "IMP_pH", "M_pH")
mydata <- mydata[, keep.var]              
mydata.cor <- cor(mydata)
par(mar = c(2,2,6.1,1))
corrplot(mydata.cor, method = "square", tl.cex=0.75, tl.col = "black")

#####################
#  Model Building   #
#####################
#Model 1 - OLS Model
M1 <- lm(TARGET ~ ., data = mydata)
summary(M1)
sort(vif(M1),decreasing=TRUE) 
AIC(M1)
BIC(M1)
mae.M1 <- mean(abs(M1$residuals));
mae.M1

M1.1 <- lm(TARGET ~ IMP_AcidIndex + IMP_Alcohol + IMP_STARS + M_STARS 
           + LabelAppeal + IMP_VolatileAcidity, data = mydata)
summary(M1.1)
sort(vif(M1.1),decreasing=TRUE) 
AIC(M1.1)
BIC(M1.1)
mae.M1.1 <- mean(abs(M1.1$residuals))
mae.M1.1
par(mar = c(5,4,4,2)+0.1)
par(mfrow = c(1, 2))
plot(M1.1$fitted.values, M1.1$residuals, xlab = "Fitted Values", ylab = "Residuals")
min(M1.1$fitted.values)
qqnorm(M1.1$residuals, col = "blue", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
qqline(M1.1$residuals)

#Model 2 - Poisson Regression
pois <- glm(TARGET ~ IMP_AcidIndex + IMP_Alcohol + IMP_STARS + M_STARS 
            + LabelAppeal + IMP_VolatileAcidity, family = "poisson", data = mydata)
summary(pois)
AIC(pois)
BIC(pois)
pois.residuals <- pois$fitted-mydata$TARGET
mae.pois <- mean(abs(pois.residuals))
mae.pois
min(pois$fitted)
max(pois$fitted)
par(mfrow = c(1, 2))
plot(pois$fitted.values, pois.residuals, xlab = "Fitted Values", ylab = "Residuals")
qqnorm(pois.residuals, col = "blue", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
qqline(pois.residuals)

#Model 3 - Negative Binomial Regression Model
nbin <- glm.nb(TARGET ~ IMP_AcidIndex + IMP_Alcohol + IMP_STARS + M_STARS 
               + LabelAppeal + IMP_VolatileAcidity, data = mydata)
summary(nbin)
AIC(nbin)
BIC(nbin)
nbin.residuals <- nbin$fitted-mydata$TARGET
mae.nbin <- mean(abs(nbin.residuals))
mae.nbin
min(nbin$fitted)
max(nbin$fitted)
par(mfrow = c(1, 2))
plot(nbin$fitted.values, nbin.residuals, xlab = "Fitted Values", ylab = "Residuals")
qqnorm(nbin.residuals, col = "blue", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
qqline(nbin.residuals)

#Model 4 Zero Inflated Poisson Regression
zip <- zeroinfl(formula = TARGET ~ IMP_AcidIndex + IMP_Alcohol + LabelAppeal + IMP_STARS + 
                  M_STARS | IMP_AcidIndex + LabelAppeal + IMP_STARS + M_STARS , data = mydata)
summary(zip)
TEMP <- 1.182816 - 0.020987*mydata$IMP_AcidIndex + 0.006860*mydata$IMP_Alcohol + 
       0.232271*mydata$LabelAppeal + 0.105005*mydata$IMP_STARS - 0.187246*mydata$M_STARS   
TEMP2 <- -1.21903 + 0.43568*mydata$IMP_AcidIndex + 0.71827*mydata$LabelAppeal - 
        3.85689*mydata$IMP_STARS + 6.07430*mydata$M_STARS
P_SCORE_ZIP_ALL <- exp(TEMP)
P_SCORE_ZERO <- exp(TEMP2)/(1+exp(TEMP2))
P_SCORE_ZIP <- P_SCORE_ZIP_ALL*(1-P_SCORE_ZERO)
ZIP.residuals <- P_SCORE_ZIP-mydata$TARGET
mae.zip <- mean(abs(ZIP.residuals))
mae.zip
plot(P_SCORE_ZIP, ZIP.residuals, xlab = "Fitted Values", ylab = "Residuals")
qqnorm(ZIP.residuals, col = "blue", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
qqline(ZIP.residuals)

#Model 5 Zero inflated Negative Binomial Regression
zinb <- zeroinfl(formula = TARGET ~ IMP_AcidIndex + IMP_Alcohol + LabelAppeal + IMP_STARS + 
                   M_STARS | IMP_AcidIndex + LabelAppeal + IMP_STARS + M_STARS, 
                 data = mydata, dist="negbin", EM = TRUE)
summary(zinb)
TEMP <- 1.182818 - 0.020987*mydata$IMP_AcidIndex + 0.006860*mydata$IMP_Alcohol + 
  0.232271*mydata$LabelAppeal + 0.105006*mydata$IMP_STARS - 0.187246*mydata$M_STARS   
TEMP2 <- -1.21873 + 0.43568*mydata$IMP_AcidIndex + 0.71827*mydata$LabelAppeal - 
  3.85717*mydata$IMP_STARS + 6.07460*mydata$M_STARS
P_SCORE_ZINB_ALL <- exp(TEMP)
P_SCORE_ZEROB <- exp(TEMP2)/(1+exp(TEMP2))
P_SCORE_ZINB <- P_SCORE_ZIP_ALL*(1-P_SCORE_ZEROB)
ZINB.residuals <- P_SCORE_ZINB-mydata$TARGET
mae.zinb <- mean(abs(ZINB.residuals))
mae.zinb
plot(P_SCORE_ZINB, ZINB.residuals, xlab = "Fitted Values", ylab = "Residuals")
qqnorm(ZINB.residuals, col = "blue", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
qqline(ZINB.residuals)

#Compare distributions of fitted values
par(mfrow = c(3, 2))
hist(mydata$TARGET, col = "blue", main = "Histogram of TARGET", xlab = "Sample Cases", xlim=c(0,8), ylim=c(0,4000))
hist(round(M1.1$fitted.values), main = "Model 1.1 Histogram", xlab = "Model 1 Fitted Values", xlim=c(0,8), ylim=c(0,4000))
hist(round(pois$fitted.values), main = "Poisson Histogram", xlab = "Poisson Fitted Values", xlim=c(0,8), ylim=c(0,4000))
hist(round(nbin$fitted.values), main = "Negative Binomial Histogram", xlab = "Negative Binomial Fitted Values", xlim=c(0,8), ylim=c(0,4000))
hist(round(P_SCORE_ZIP), main = "ZIP Histogram", xlab = "ZIP Fitted Values", xlim=c(0,8), ylim=c(0,4000))
hist(round(P_SCORE_ZINB), main = "ZINB Histogram", xlab = "ZINB Fitted Values", xlim=c(0,8), ylim=c(0,4000))






##################################################################
##################################################################
########   Section 2: Scoring Program using ZIP Model 
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

#Read in wine test dataset. File path will require changing to 
#where you have stored the file on your computer.
path.name <- 'C:/Users/mkdob/OneDrive/Northwestern/411 GLM/Assignment 3/';
file.name <- paste(path.name,'wine_test.csv', sep='');
mydata <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);
colnames(mydata)[1] <- "INDEX"


#############################
#Data Prep
#############################
#Acid Index - max cutoff, replace with mean
mydata$AcidIndex <-ifelse(mydata$AcidIndex > 14 ,NA, mydata$AcidIndex)
MEEN <- mean(mydata$AcidIndex, na.rm=TRUE)
mydata$M_AcidIndex <- ifelse(is.na(mydata$AcidIndex), 1, 0)
mydata$IMP_AcidIndex <- ifelse(is.na(mydata$AcidIndex), MEEN, mydata$AcidIndex)

#Alcohol - Use absolute values for negative values. Use mean for missing values. 
mydata$M_Alcohol <- ifelse(is.na(mydata$Alcohol),  1, ifelse(mydata$Alcohol < 0, 1, 0))
mydata$IMP_Alcohol <- abs(mydata$Alcohol)
MEEN <- mean(mydata$IMP_Alcohol, na.rm=TRUE)
mydata$IMP_Alcohol <- ifelse(is.na(mydata$IMP_Alcohol), MEEN, mydata$IMP_Alcohol)

#Chlorides - Use absolute values for negative values. Use mean missing values. 
mydata$M_Chlorides <- ifelse(is.na(mydata$Chlorides),  1, ifelse(mydata$Chlorides < 0, 1, 0))
mydata$IMP_Chlorides <- abs(mydata$Chlorides)
MEEN <- mean(mydata$IMP_Chlorides, na.rm=TRUE)
mydata$IMP_Chlorides <- ifelse(is.na(mydata$IMP_Chlorides), MEEN, mydata$IMP_Chlorides)

#CitricAcid - Use absolute values for negative values.
mydata$M_CitricAcid <- ifelse(is.na(mydata$CitricAcid),  1, ifelse(mydata$CitricAcid < 0, 1, 0))
mydata$IMP_CitricAcid <- abs(mydata$CitricAcid)

#Density - No treatment required

#FixedAcidity - Use absolute values for negative values.
mydata$M_FixedAcidity <- ifelse(is.na(mydata$FixedAcidity),  1, ifelse(mydata$FixedAcidity < 0, 1, 0))
mydata$IMP_FixedAcidity <- abs(mydata$FixedAcidity)

#FreeSulfurDioxide - Use absolute values for negative values. Use mean missing values. Maximum cutoff 500.
mydata$M_FreeSulfurDioxide <- ifelse(is.na(mydata$FreeSulfurDioxide), 1, 
                                     ifelse(mydata$FreeSulfurDioxide < 0, 1, 
                                            ifelse(mydata$FreeSulfurDioxide > 500, 1, 0)))
mydata$FreeSulfurDioxide <- abs(mydata$FreeSulfurDioxide)
mydata$FreeSulfurDioxide <-ifelse(mydata$FreeSulfurDioxide > 500 ,NA, mydata$FreeSulfurDioxide)
MEEN <- mean(mydata$FreeSulfurDioxide, na.rm=TRUE)
mydata$IMP_FreeSulfurDioxide <- ifelse(is.na(mydata$FreeSulfurDioxide), MEEN, mydata$FreeSulfurDioxide)

#LabelAppeal - No treatment required

#ResidualSugar - Use absolute values for negative values. Use mean missing values. 
mydata$M_ResidualSugar <- ifelse(is.na(mydata$ResidualSugar),  1, ifelse(mydata$ResidualSugar < 0, 1, 0))
mydata$IMP_ResidualSugar <- abs(mydata$ResidualSugar)
MEEN <- mean(mydata$IMP_ResidualSugar, na.rm=TRUE)
mydata$IMP_ResidualSugar <- ifelse(is.na(mydata$IMP_ResidualSugar), MEEN, mydata$IMP_ResidualSugar)

#STARS - Use mean missing values
mydata$M_STARS <- ifelse(is.na(mydata$STARS),  1, 0)
MEEN <- mean(mydata$STARS, na.rm=TRUE)
mydata$IMP_STARS <- ifelse(is.na(mydata$STARS), MEEN, mydata$STARS)

#Sulphates -Use absolute values for negative values. Use mean missing values. 
mydata$M_Sulphates <- ifelse(is.na(mydata$Sulphates),  1, ifelse(mydata$Sulphates < 0, 1, 0))
mydata$IMP_Sulphates <- abs(mydata$Sulphates)
MEEN <- mean(mydata$IMP_Sulphates, na.rm=TRUE)
mydata$IMP_Sulphates <- ifelse(is.na(mydata$IMP_Sulphates), MEEN, mydata$IMP_Sulphates)

#TotalSulfurDioxide - Use absolute values for negative values. Use mean missing values. Maximum cutoff 500.
mydata$M_TotalSulfurDioxide <- ifelse(is.na(mydata$TotalSulfurDioxide), 1, 
                                      ifelse(mydata$TotalSulfurDioxide < 0, 1, 
                                             ifelse(mydata$TotalSulfurDioxide > 500, 1, 0)))
mydata$TotalSulfurDioxide <- abs(mydata$TotalSulfurDioxide)
mydata$TotalSulfurDioxide <-ifelse(mydata$TotalSulfurDioxide > 500 ,NA, mydata$TotalSulfurDioxide)
MEEN <- mean(mydata$TotalSulfurDioxide, na.rm=TRUE)
mydata$IMP_TotalSulfurDioxide <- ifelse(is.na(mydata$TotalSulfurDioxide), MEEN, mydata$TotalSulfurDioxide)

#VolatileAcidity - Use absolute values for negative values.
mydata$M_VolatileAcidity <- ifelse(is.na(mydata$VolatileAcidity),  1, ifelse(mydata$VolatileAcidity < 0, 1, 0))
mydata$IMP_VolatileAcidity <- abs(mydata$VolatileAcidity)

#pH - Use mean missing values. Minimum cutoff 2,  Maximum cutoff 5.
mydata$M_pH <- ifelse(is.na(mydata$pH), 1, ifelse(mydata$pH < 2, 1, ifelse(mydata$pH > 5, 1, 0)))
mydata$pH <-ifelse(mydata$pH < 2 ,NA, mydata$pH)
mydata$pH <-ifelse(mydata$pH > 5 ,NA, mydata$pH)
MEEN <- mean(mydata$pH, na.rm=TRUE)
mydata$IMP_pH <- ifelse(is.na(mydata$pH), MEEN, mydata$pH)

# Update mydata dataframe with the variables to be used in the models
keep.var<- c("TARGET", "INDEX", "IMP_AcidIndex","IMP_CitricAcid", "IMP_FreeSulfurDioxide", "IMP_Sulphates",
             "M_AcidIndex", "M_CitricAcid", "M_FreeSulfurDioxide", "M_Sulphates",
             "IMP_Alcohol", "Density", "IMP_ResidualSugar", "IMP_TotalSulfurDioxide",
             "M_Alcohol", "LabelAppeal", "M_ResidualSugar", "M_TotalSulfurDioxide",
             "IMP_Chlorides", "IMP_FixedAcidity", "IMP_STARS", "IMP_VolatileAcidity",
             "M_Chlorides", "M_FixedAcidity", "M_STARS",	"M_VolatileAcidity",
             "IMP_pH", "M_pH")
mydata <- mydata[, keep.var]  

#Harded coded ZIP Model
TEMP <- 1.182816 - 0.020987*mydata$IMP_AcidIndex + 0.006860*mydata$IMP_Alcohol + 
  0.232271*mydata$LabelAppeal + 0.105005*mydata$IMP_STARS - 0.187246*mydata$M_STARS   
TEMP2 <- -1.21903 + 0.43568*mydata$IMP_AcidIndex + 0.71827*mydata$LabelAppeal - 
  3.85689*mydata$IMP_STARS + 6.07430*mydata$M_STARS
P_SCORE_ZIP_ALL <- exp(TEMP)
P_SCORE_ZERO <- exp(TEMP2)/(1+exp(TEMP2))
P_SCORE_ZIP <- P_SCORE_ZIP_ALL*(1-P_SCORE_ZERO)

#Output predictions into CSV file.
P_TARGET <- round(P_SCORE_ZIP)
mydata2 <- data.frame(mydata$INDEX, P_TARGET)
colnames(mydata2)[1] <- "INDEX"
path.name <- 'C:/Users/mkdob/OneDrive/Northwestern/411 GLM/Assignment 3/';
file.name <- paste(path.name,'Dobbin_Predicted.csv', sep='');
write.csv(mydata2, file = file.name)


















































