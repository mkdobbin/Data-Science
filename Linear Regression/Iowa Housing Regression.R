#Matthew Dobbin
#Ames Sample Data with drop conditons
#08/07/2017
library(MASS)

# Read in csv file for Ames housing data;
path.name <- 'C:/Users/mkdob/OneDrive/Northwestern/410 Multi/Data/';
file.name <- paste(path.name,'ames_housing_data.csv', sep='');

# Read in the csv file into an R data frame;
ames.df <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);

ames.df$dropCondition <- ifelse(ames.df$BldgType != '1Fam','01: Not 1Fam',
                         ifelse(ames.df$Zoning!='RL','02: Not RL',
                         ifelse(ames.df$SaleCondition!='Normal','03: Non-Normal Sale',
                         ifelse(ames.df$Street!='Pave','04: Street Not Paved',
                         ifelse(ames.df$YearBuilt <1950,'05: Built Pre-1950',
                         ifelse(ames.df$GrLivArea >4000,'06: GRLiv > 4000 SqFt',
                         ifelse(ames.df$GrLivArea <800,'07: GRLiv < 800 SqFt',
                         ifelse(ames.df$LotArea >100000,'08: LA < 100000 SqFT',
                         ifelse(ames.df$BedroomAbvGr < 1,'09: Zero Beds',
                         ifelse(ames.df$TotalBsmtSF <1,'10: No Basement',      
                         ifelse(ames.df$GarageArea < 1 ,'11: No Garage',
                         ifelse(ames.df$LotFrontage > 300 ,'12: LF > 300 SqFte',
                         '99: Eligible Sample'))))))))))));

table(ames.df$dropCondition)
waterfall <- table(ames.df$dropCondition);
as.matrix(waterfall,11,1)
el.pop <- subset(ames.df,dropCondition=='99: Eligible Sample');
table(el.pop$dropCondition)        
str(el.pop)

#Omit NAs
keep.vars <- c('SID','PID','LotFrontage','LotArea','LotConfig','Neighborhood','MasVnrArea',
               'LowQualFinSF', 'MiscVal', 'BsmtUnfSF', 
               'HouseStyle','OverallQual','OverallCond','YearBuilt','YearRemodel','Exterior1',
               'BsmtFinSF1','BsmtFinSF2','CentralAir','GrLivArea','BsmtFullBath','BsmtHalfBath',
               'FullBath','HalfBath','BedroomAbvGr','TotRmsAbvGrd','Fireplaces','GarageCars',
               'GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','ThreeSsnPorch',
               'ScreenPorch','PoolArea','MoSold',
               'YrSold','SaleCondition','SalePrice', 'Heating', 'BldgType', 'TotalBsmtSF', 
               'GarageType', 'Condition1');

skinny.df <- el.pop[,keep.vars];
str(skinny.df)
sample.df <- na.omit(skinny.df);
str(sample.df)

#Create calculated variable
sample.df$TotalBathCalc <- sample.df$BsmtFullBath + 0.5*sample.df$BsmtHalfBath + sample.df$FullBath + 0.5*sample.df$HalfBath;
sample.df$TotalSqftCalc <- sample.df$BsmtFinSF1+sample.df$BsmtFinSF2+sample.df$GrLivArea;
sample.df$QualityIndex <- sample.df$OverallQual*sample.df$OverallCond;

#Create indicator variables
sample.df$garage1 <- ifelse(sample.df$GarageCars==1,1,0);
sample.df$garage2 <- ifelse(sample.df$GarageCars==2,1,0);
sample.df$garage3 <- ifelse(sample.df$GarageCars>=3,1,0);
sample.df$CornerLotInd <- ifelse(sample.df$LotConfig=='Corner',1,0);
sample.df$FireplaceInd1 <- ifelse((sample.df$Fireplaces>0)&(sample.df$Fireplaces<2),1,0);
sample.df$FireplaceInd2 <- ifelse((sample.df$Fireplaces>1),1,0);
sample.df$PoolInd <- ifelse(sample.df$PoolArea>0,1,0);
sample.df$I2007 <- ifelse(sample.df$YrSold==2007,1,0);
sample.df$I2008 <- ifelse(sample.df$YrSold==2008,1,0);
sample.df$I2009 <- ifelse(sample.df$YrSold==2009,1,0);
sample.df$I2010 <- ifelse(sample.df$YrSold==2010,1,0);

#Set the Seed
set.seed(123)
sample.df$u <- runif(n=dim(sample.df)[1], min = 0, max = 1);

#Create train/test plit;
train.df <- subset(sample.df, u < 0.70);
test.df <- subset(sample.df, u >= 0.70);
dim(sample.df)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

saveRDS(sample.df,file='C:/Users/mkdob/Desktop/Northwestern/410 Multi/Data/ames_sample.RData');

###############################
#Model Identification by Automated Variable Selection
###############################

#Create clean training sample
drop.list <- c('SID','PID','LotConfig','HouseStyle','OverallQual','OverallCond',
               'YearBuilt','YearRemodel','Exterior1', 'Neighborhood',
               'BsmtFinSF1','BsmtFinSF2','CentralAir','BsmtFullBath','BsmtHalfBath',
               'FullBath','HalfBath','BedroomAbvGr','TotRmsAbvGrd','Fireplaces','GarageCars',
               'EnclosedPorch','ThreeSsnPorch', 'ScreenPorch','MoSold','YrSold','SaleCondition',
               'Heating', 'BldgType', 'TotalBsmtSF', 'GarageType', 'Condition1', 'u'); 

train.clean <-train.df[,!(names(sample.df) %in% drop.list)];
str(train.clean)

#Identify (list the four models)
forward.lm <- list()
backward.lm <- list()
stepwise.lm <- list()
sqft.lm <- list()
junk.lm <- list()

# Define the upper model as the FULL model 
upper.lm <- lm(SalePrice ~ .,data=train.clean); 
summary(upper.lm) 

# Define the lower model as the Intercept model 
lower.lm <- lm(SalePrice ~ 1,data=train.clean); 

# Need a SLR to initialize stepwise selection 
sqft.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean); 
summary(sqft.lm)

#Call stepAIC() for variable selection
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=~1), direction = c ('forward'));
summary(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward')); 
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1), direction=c('both'));
summary(stepwise.lm)

junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea +  TotalSqftCalc, data=train.df) 
summary(junk.lm) 

# Compute the VIF values 
library(car) 
sort(vif(forward.lm),decreasing=TRUE) 
sort(vif(backward.lm),decreasing=TRUE) 
sort(vif(stepwise.lm),decreasing=TRUE) 
sort(vif(junk.lm),decreasing=TRUE) 

#Compute the AIC & BIC values for each model
AIC(forward.lm)
BIC(forward.lm)
AIC(backward.lm)
BIC(backward.lm)
AIC(stepwise.lm)
BIC(stepwise.lm)
AIC(junk.lm)
BIC(junk.lm)

#Compute the MSE & MAE for each model
mse.forward <- mean(forward.lm$residuals^2);
mae.forward <- mean(abs(forward.lm$residuals));
mse.forward
mae.forward

mse.backward <- mean(backward.lm$residuals^2);
mae.backward <- mean(abs(backward.lm$residuals));
mse.backward
mae.backward

mse.stepwise <- mean(stepwise.lm$residuals^2);
mae.stepwise <- mean(abs(stepwise.lm$residuals));
mse.stepwise 
mae.stepwise

mse.junk <- mean(junk.lm$residuals^2);
mae.junk <- mean(abs(junk.lm$residuals));
mse.junk
mae.junk

#######################
#Predictive Accurarcy
forward.test <- predict(forward.lm, newdata=test.df); 
mse.forward.test <- mean((test.df$SalePrice-forward.test)^2);
mae.forward.test <- mean(abs(test.df$SalePrice-forward.test));
mse.forward.test
mae.forward.test

backward.test <- predict(backward.lm, newdata=test.df); 
mse.backward.test <- mean((test.df$SalePrice-backward.test)^2);
mae.backward.test <- mean(abs(test.df$SalePrice-backward.test));
mse.backward.test
mae.backward.test

stepwise.test <- predict(stepwise.lm, newdata=test.df); 
mse.stepwise.test <- mean((test.df$SalePrice-stepwise.test)^2);
mae.stepwise.test <- mean(abs(test.df$SalePrice-stepwise.test));
mse.stepwise.test
mae.stepwise.test

junk.test <- predict(junk.lm, newdata=test.df); 
mse.junk.test <- mean((test.df$SalePrice-junk.test)^2);
mae.junk.test <- mean(abs(test.df$SalePrice-junk.test));
mse.junk.test
mae.junk.test

######

##############
#Operational Validation
##############
# Training Data 
# Abs Pct Error 
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice; 

# Assign Prediction Grades; 
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]', 
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]', 
                                                'Grade 4: (0.25+]') 
                                  ) 
) 

forward.trainTable <- table(forward.PredictionGrade) 
forward.trainTable/sum(forward.trainTable) 

# Test Data 
# Abs Pct Error 
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice; 

# Assign Prediction Grades; 
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]', 
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]', 
                                                    'Grade 4: (0.25+]') 
                                      ) 
) 

forward.testTable <-table(forward.testPredictionGrade) 
forward.testTable/sum(forward.testTable) 

##########################
#backward
# Abs Pct Error 
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice; 

# Assign Prediction Grades; 
backward.PredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0.0.10]', 
                                  ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                         ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]', 
                                                'Grade 4: (0.25+]') 
                                  ) 
) 

backward.trainTable <- table(backward.PredictionGrade) 
backward.trainTable/sum(backward.trainTable) 

# Test Data 
# Abs Pct Error 
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice; 

# Assign Prediction Grades; 
backward.testPredictionGrade <- ifelse(backward.testPCT<=0.10,'Grade 1: [0.0.10]', 
                                      ifelse(backward.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                             ifelse(backward.testPCT<=0.25,'Grade 3: (0.15,0.25]', 
                                                    'Grade 4: (0.25+]') 
                                      ) 
) 

backward.testTable <-table(backward.testPredictionGrade) 
backward.testTable/sum(backward.testTable) 

###########################
#Step-wise
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice; 

# Assign Prediction Grades; 
stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0.0.10]', 
                                  ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                         ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]', 
                                                'Grade 4: (0.25+]') 
                                  ) 
) 

stepwise.trainTable <- table(stepwise.PredictionGrade) 
stepwise.trainTable/sum(stepwise.trainTable) 

# Test Data 
# Abs Pct Error 
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice; 

# Assign Prediction Grades; 
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT<=0.10,'Grade 1: [0.0.10]', 
                                      ifelse(stepwise.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                             ifelse(stepwise.testPCT<=0.25,'Grade 3: (0.15,0.25]', 
                                                    'Grade 4: (0.25+]') 
                                      ) 
) 

stepwise.testTable <-table(stepwise.testPredictionGrade) 
stepwise.testTable/sum(stepwise.testTable) 

###########################
#Junk 
# Abs Pct Error 
junk.pct <- abs(junk.lm$residuals)/train.clean$SalePrice; 

# Assign Prediction Grades; 
junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0.0.10]', 
                                  ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]', 
                                         ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]', 
                                                'Grade 4: (0.25+]') 
                                  ) 
) 

junk.trainTable <- table(junk.PredictionGrade) 
junk.trainTable/sum(junk.trainTable) 

# Test Data 
# Abs Pct Error 
junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice; 

# Assign Prediction Grades; 
junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]', 
                                      ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]', 
                                             ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]', 
                                                    'Grade 4: (0.25+]') 
                                      ) 
) 

junk.testTable <-table(junk.testPredictionGrade) 
junk.testTable/sum(junk.testTable) 














