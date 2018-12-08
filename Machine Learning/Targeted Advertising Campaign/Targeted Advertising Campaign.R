# PREDICT 422 Practical Machine Learning
# Matthew Dobbin
# 26/02/2018

# Course Project 

# OBJECTIVE: A charitable organization wishes to develop a machine learning
# model to improve the cost-effectiveness of their direct marketing campaigns
# to previous donors.

# 1) Develop a classification model using data from the most recent campaign that
# can effectively capture likely donors so that the expected net profit is maximized.

# 2) Develop a prediction model to predict donation amounts for donors - the data
# for this will consist of the records for donors only.

# load the data
path.name <- 'C:/Users/mkdob/OneDrive/Northwestern/422 Machine Learning/Project/';
file.name <- paste(path.name,'charity.csv', sep='');

# Read in the csv file into an R data frame;# load the "charity.csv" file
charity <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);

#######################################################################################
# Exploratory Data Analysis 
#######################################################################################
str(charity) 
summary(charity)
hist(charity$damt, col = "blue", main = "Histogram of DAMT", xlab = "Donation amount ($)")
hist(charity$donr, col = "blue", main = "Histogram of DAMT", xlab = "Donation amount ($)")
aggregate(damt~damt >0,charity,mean)

#Summary Statistics & Data Transformations
library(lessR)
library(rcompanion)
library(MASS)
library(caret)
library(car)
library(tree)
library(e1071)
library(class)
par(mfrow = c(1,2))

#reg1 0/1
SummaryStats(reg1, data = charity)
hist(charity$reg1)
boxplot(charity$reg1)
table(charity$reg1)

#reg2 0/1
SummaryStats(reg2, data = charity)
hist(charity$reg2)
boxplot(charity$reg2)
table(charity$reg2)

#reg3 0/1
SummaryStats(reg3, data = charity)
hist(charity$reg3)
boxplot(charity$reg3)
table(charity$reg3)

#reg4 0/1
SummaryStats(reg4, data = charity)
hist(charity$reg4)
boxplot(charity$reg4)
table(charity$reg4)

#home 0/1
SummaryStats(home, data = charity)
hist(charity$home)
boxplot(charity$home)
table(charity$home)

#CHLD  - Transform Categorical <1, < 4
SummaryStats(chld, data = charity)
hist(charity$chld)
boxplot(charity$chld)
table(charity$chld)
charity$G0_chld <- ifelse(charity$chld > 0 & charity$chld <= 3, 1, 0)
charity$G3_chld <- ifelse(charity$chld > 3, 1, 0)

#HINC - Leave untransformed 
SummaryStats(hinc, data = charity)
hist(charity$hinc)
boxplot(charity$hinc)
table(charity$hinc)
Box = boxcox(charity$hinc ~ 1, lambda = seq(-6,6,0.1))              
Cox = data.frame(Box$x, Box$y)            
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] 
Cox2[1,]   
lambda = Cox2[1, "Box.x"]                 
charity$T_hinc = (charity$hinc**lambda - 1)/lambda   
plotNormalHistogram(charity$hinc, main="No transformation")
plotNormalHistogram(charity$T_hinc, main=paste("lambda =", round(lambda, 2)))

#GENF - 0/1
SummaryStats(genf, data = charity)
hist(charity$genf)
boxplot(charity$genf)
table(charity$genf)

#WRAT - Transform - Chose Categorical >2, >7
SummaryStats(wrat, data = charity)
hist(charity$wrat)
boxplot(charity$wrat)
table(charity$wrat)
charity$G2_wrat <- ifelse(charity$wrat > 2 & charity$wrat <=7, 1, 0)
charity$G7_wrat <- ifelse(charity$wrat > 7, 1, 0)

#AVHV - Transform Lambda = -0.1
SummaryStats(avhv, data = charity)
hist(charity$avhv)
boxplot(charity$avhv)
Box = boxcox(charity$avhv ~ 1, lambda = seq(-6,6,0.1))              
Cox = data.frame(Box$x, Box$y)            
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] 
Cox2[1,]   
lambda = Cox2[1, "Box.x"]                 
charity$T_avhv = (charity$avhv**lambda - 1)/lambda   
plotNormalHistogram(charity$avhv, main="No transformation")
plotNormalHistogram(charity$T_avhv, main=paste("lambda =", round(lambda, 2)))

#INCM - Log Transform 
SummaryStats(incm, data = charity)
hist(charity$incm)
boxplot(charity$incm)
Box = boxcox(charity$incm ~ 1, lambda = seq(-6,6,0.1))              
Cox = data.frame(Box$x, Box$y)            
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] 
Cox2[1,]   
lambda = Cox2[1, "Box.x"]                 
charity$T_incm = log(charity$incm)
plotNormalHistogram(charity$incm, main="No transformation")
plotNormalHistogram(charity$T_incm, main=paste("lambda =", round(lambda, 2)))

#INCA - Transform LAMBDA = -0.1
SummaryStats(inca, data = charity)
hist(charity$inca)
boxplot(charity$inca)
Box = boxcox(charity$inca ~ 1, lambda = seq(-6,6,0.1))              
Cox = data.frame(Box$x, Box$y)            
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] 
Cox2[1,]   
lambda = Cox2[1, "Box.x"]                 
charity$T_inca = (charity$inca**lambda - 1)/lambda
plotNormalHistogram(charity$inca, main="No transformation")
plotNormalHistogram(charity$T_inca, main=paste("lambda =", round(lambda, 2)))

#plow - Transform
SummaryStats(plow, data = charity)
hist(charity$plow)
boxplot(charity$plow)
charity$G0_plow <- ifelse(charity$plow > 0 & charity$plow <=4, 1, 0)
charity$G4_plow <- ifelse(charity$plow > 4 & charity$plow <=21, 1, 0)
charity$G21_plow <- ifelse(charity$plow > 21, 1, 0)

#npro - Transformed lamba = 0.6
SummaryStats(npro, data = charity)
hist(charity$npro)
boxplot(charity$npro)
Box = boxcox(charity$npro ~ 1, lambda = seq(-6,6,0.1))              
Cox = data.frame(Box$x, Box$y)            
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] 
Cox2[1,]   
lambda = Cox2[1, "Box.x"]                 
charity$T_npro = (charity$npro**lambda - 1)/lambda
plotNormalHistogram(charity$npro, main="No transformation")
plotNormalHistogram(charity$T_npro, main=paste("lambda =", round(lambda, 2)))

#tgif - Transform Lambda =-0.3
SummaryStats(tgif, data = charity)
hist(charity$tgif)
boxplot(charity$tgif)
table(charity$tgif)
Box = boxcox(charity$tgif ~ 1, lambda = seq(-6,6,0.1))              
Cox = data.frame(Box$x, Box$y)            
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] 
Cox2[1,]   
lambda = Cox2[1, "Box.x"]                 
charity$T_tgif = (charity$tgif**lambda - 1)/lambda
plotNormalHistogram(charity$tgif, main="No transformation")
plotNormalHistogram(charity$T_tgif, main=paste("lambda =", round(lambda, 2)))

#lgif - Transform Lambda =-0.2
SummaryStats(lgif, data = charity)
hist(charity$lgif)
boxplot(charity$lgif)
table(charity$lgif)
Box = boxcox(charity$lgif ~ 1, lambda = seq(-6,6,0.1))              
Cox = data.frame(Box$x, Box$y)            
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] 
Cox2[1,]   
lambda = Cox2[1, "Box.x"]                 
charity$T_lgif = (charity$lgif**lambda - 1)/lambda
plotNormalHistogram(charity$lgif, main="No transformation")
plotNormalHistogram(charity$T_lgif, main=paste("lambda =", round(lambda, 2)))

#rgif - Log transformation
SummaryStats(rgif, data = charity)
hist(charity$rgif)
boxplot(charity$rgif)
table(charity$rgif)
Box = boxcox(charity$rgif ~ 1, lambda = seq(-6,6,0.1))              
Cox = data.frame(Box$x, Box$y)            
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] 
Cox2[1,]   
lambda = Cox2[1, "Box.x"]                 
charity$T_rgif = log(charity$rgif)
plotNormalHistogram(charity$rgif, main="No transformation")
plotNormalHistogram(charity$T_rgif, main=paste("lambda =", round(lambda, 2)))

#tdon - Leave untransformed
SummaryStats(tdon, data = charity)
hist(charity$tdon)
boxplot(charity$tdon)
table(charity$tdon)

#tlag - Indicator vairables
SummaryStats(tlag, data = charity)
hist(charity$tlag)
boxplot(charity$tlag)
table(charity$tlag)
charity$G2_tlag <- ifelse(charity$tlag > 2 & charity$tlag <=7, 1, 0)
charity$G7_tlag <- ifelse(charity$tlag > 7, 1, 0)

#agif - LOG TRANSFORMATION
SummaryStats(agif, data = charity)
hist(charity$agif)
boxplot(charity$agif)
table(charity$agif)
Box = boxcox(charity$agif ~ 1, lambda = seq(-6,6,0.1))              
Cox = data.frame(Box$x, Box$y)            
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] 
Cox2[1,]   
lambda = Cox2[1, "Box.x"]                 
charity$T_agif = log(charity$agif)
plotNormalHistogram(charity$agif, main="No transformation")
plotNormalHistogram(charity$T_agif, main=paste("lambda =", round(lambda, 2)))

######################################################################################
#Set up dataframe with transformed data variables and split into train, validation, test
keep.vars <- c('ID', 'reg1', 'reg2', 'reg3', 'reg4', 'home', 'chld', 'G0_chld', 'G3_chld', 
               'hinc', 'T_hinc', 'genf', 'wrat', 'G2_wrat', 'G7_wrat', 'avhv', 'T_avhv', 
               'incm', 'T_incm', 'inca', 'T_inca', 'plow',   'G0_plow', 'G4_plow', 'G21_plow', 
               'npro', 'T_npro', 'tgif', 'T_tgif', 'lgif', 'T_lgif', 'rgif', 'T_rgif', 'tdon',
               'tlag' ,'G2_tlag', 'G7_tlag', 'agif', 'T_agif', 'donr', 'damt', 'part')
charity.t <- charity[,keep.vars]
str(charity)
str(charity.t)

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:39]
c.train <- data.train[,40] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,41] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:39]
c.valid <- data.valid[,40] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,41] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:39]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

#####################################################################################################
##### CLASSIFICATION MODELING #######################################################################
#####################################################################################################
par(mfrow = c(1,1))

########## linear discriminant analysis ##########################
model.lda1 <- lda(donr ~ reg1 + reg2 +  reg4 + home + G0_chld + G3_chld +  I(hinc^2) + genf + wrat + 
                    T_avhv + incm + inca + G0_plow  + G4_plow  + G21_plow + npro + tgif + rgif + 
                    tdon + I(tdon^2) + G2_tlag + G7_tlag, data.train.std.c) 
post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1418 11374
cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 710  11
#              1 309 988
# check n.mail.valid = 309+988 = 1297
# check profit = 14.5*988-2*1297 = 11732

############ logistic regression ########################
model.log1 <- glm(donr ~ reg1 + reg2 + reg4 + home + G0_chld + G3_chld +  I(hinc^2) + genf + wrat + 
                    T_avhv + incm + inca + G0_plow  + G4_plow  + G21_plow + npro + tgif + rgif + 
                    tdon + I(tdon^2) + G2_tlag + G7_tlag, 
                  data.train.std.c, family=binomial("logit"))
post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1313 11772.5
cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 699   6
#              1 320 993
# check n.mail.valid = 320+993 = 1313
# check profit = 14.5*993-2*1313 = 11772.5

######################## Quadratic discriminant analysis ##################
model.qda1 <- qda(donr ~ reg1 + reg2 +  reg4 + home + G0_chld + G3_chld + hinc + I(hinc^2) +  wrat 
                    + incm + T_inca + plow +  tgif +  tdon + I(tdon^2) + G2_tlag + G7_tlag + agif, data.train.std.c) 
post.valid.qda1 <- predict(model.qda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.qda1 <- cumsum(14.5*c.valid[order(post.valid.qda1, decreasing=T)]-2)
plot(profit.qda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.qda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.qda1)) # report number of mailings and maximum profit
# 1579 11269.5
cutoff.qda1 <- sort(post.valid.qda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.qda1 <- ifelse(post.valid.qda1>cutoff.qda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.qda1, c.valid) # classification table
#               c.valid
#chat.valid.qda1   0   1
#              0 435  4
#              1 584 995
# check n.mail.valid = 584+995 = 1579
# check profit = 14.5*995-2*1579 = 11269.5

############  Random Forest Bagged #################
library(randomForest)
data.train.std.c$tree_donr <- ifelse(data.train.std.c$donr == 1, "YES", "NO")
data.train.std.c$tree_donr <- factor(data.train.std.c$tree_donr)
set.seed(2)
model.tree2 = randomForest(tree_donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                            avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + 
                            tlag + agif, data = data.train.std.c, mtry=20, importance= TRUE)
summary(model.tree2)
importance(model.tree2)
varImpPlot(model.tree2)
model.tree2
post.valid.tree2 <- predict(model.tree2, newdata = data.valid.std.c, type="prob")[,2]
profit.tree2 <- cumsum(14.5*c.valid[order(post.valid.tree2, decreasing=T)]-2)
plot(profit.tree2)
n.mail.valid <- which(profit.tree2 == max(profit.tree2)) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.tree2))
#1262 11715
cutoff.tree2 <- sort(post.valid.tree2, decreasing=T)[n.mail.valid+1] 
chat.valid.tree2 <- ifelse(post.valid.tree2>cutoff.tree2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.tree2, c.valid) # classification table

#                  c.valid
#chat.valid.tree2   0   1
#                0 739  19
#                1 280 980
# check n.mail.valid = 280+980 = 1260 
# check profit = 14.5*980-2*1260 = 11690
data.train.std.c$tree_donr <- NULL

#######K-Nearest Neighbors #########
model.knn1 <- knn(train = data.train.std.c[,c(1:6,9,11,12, 15,17, 19, 21, 25, 27, 29, 31, 33,34,37)], 
                  test = data.valid.std.c[,c(1:6,9,11,12, 15,17, 19, 21, 25, 27, 29, 31, 33,34,37)], 
                  cl = c.train,  k=12, prob = TRUE)
post.valid.knn1 <-attributes(model.knn1)$prob
table(round(post.valid.knn1,2))
summary(model.knn1)
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.knn1 <- cumsum(14.5*c.valid[order(post.valid.knn1, decreasing=T)]-2)
plot(profit.knn1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.knn1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.knn1)) # report number of mailings and maximum profit
# 2017 10451.5
cutoff.knn1 <- sort(post.valid.knn1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.knn1 <- ifelse(post.valid.knn1>cutoff.knn1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.knn1, c.valid) # classification table
#               c.valid
#chat.valid.knn1   0   1
#              0  90  31
#              1 929 968
# check n.mail.valid = 929+968 = 1897
# check profit = 14.5*968-2*1897 = 10242

#######Support Vector Machine ######################################
data.train.std.c$tree_donr <- ifelse(data.train.std.c$donr == 1, "YES", "NO")
data.train.std.c$tree_donr <- factor(data.train.std.c$tree_donr)
#model.svm1 <- tune(svm, tree_donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + 
#                     genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + 
#                     rgif + tdon + tlag + agif, data=data.train.std.c, kernel="radial",
#                   ranges=list(cost=c(0.1,1,10,1000), gamma=c(0.5,1,2,3,4)))
#summary(model.svm1)
model.svm2 <- svm(tree_donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + 
                    genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + 
                    rgif + tdon + tlag + agif, data=data.train.std.c, kernel="radial",
                  gamma=0.5, cost=10, probability=TRUE) 
model.svm2
post.valid.svm2 <- predict(model.svm2, newdata = data.valid.std.c, probability=TRUE)
post.valid.svm2 <- attr(post.valid.svm2, "probabilities")[,2]
profit.svm2 <- cumsum(14.5*c.valid[order(post.valid.svm2, decreasing=T)]-2)
plot(profit.svm2)
n.mail.valid <- which(profit.svm2 == max(profit.svm2)) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svm2))
#1260 11182.5
cutoff.svm2 <- sort(post.valid.svm2, decreasing=T)[n.mail.valid+1] 
chat.valid.svm2 <- ifelse(post.valid.svm2>cutoff.svm2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.svm2, c.valid) # classification table
#                 c.valid
#chat.valid.svm2    0    1
#               0  704  54
#               1  315 945
# check n.mail.valid = 315+945 = 1260
# check profit = 14.5*945-2*1260 = 11182.5
data.train.std.c$tree_donr <- NULL

##############################################################################################
# Results Clasification
# n.mail Profit   Model
# 1297    11732    LDA
# 1313  11772.5    Log
# 1579  11269.5    QDA
# 1487    11381   Tree 
# 1897    10242    KNN 
# 1260  11182.5    SVM      

# select model.log1 since it has maximum profit in the validation sample
post.test <- predict(model.log1, data.test.std, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set
n.mail.valid <- which.max(profit.log1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1663  344
# based on this model we'll mail to the 344 highest posterior probabilities
# See below for saving chat.test into a file for submission


##################################################################################################
##### PREDICTION MODELING ########################################################################
par(mfrow = c(1,1))

# Least squares regression###################
model.ls1 <- lm(damt ~ reg2 + reg3 + reg4 + home + G0_chld + G3_chld + hinc + 
                  G2_wrat + G7_wrat + incm + T_tgif + T_lgif + T_rgif + T_agif,
                  data.train.std.y)
mean((y.train - model.ls1$fitted.values)^2)
#1.35631 MSE Train
summary(model.ls1)
pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
plot(pred.valid.ls1, y.valid)
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.498506 MSE Valid
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1549062

########## PCR #########
library(pls)
set.seed(1)
model.pcr=pcr(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
              avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + 
              tlag + agif, data= data.train.std.y, scale=TRUE, validation="CV")
summary(model.pcr)
validationplot(model.pcr,val.type="MSEP")
pred.valid.pcr7=predict(model.pcr, data.valid.std.y, ncomp=7)
mean((y.valid - pred.valid.pcr7)^2)

set.seed(1)
model.pcr=pcr(damt ~ reg2 + reg3 + reg4 + home + G0_chld + G3_chld + hinc + 
                G2_wrat + G7_wrat + incm + T_tgif + T_lgif + T_rgif + T_agif, 
                data= data.train.std.y, scale=TRUE, validation="CV")
model.pcr
summary(model.pcr)
validationplot(model.pcr,val.type="MSEP")
pred.valid.pcr11=predict(model.pcr, data.valid.std.y, ncomp=11)
mean((y.valid - pred.valid.pcr11)^2)
#1.569526


########### Tree ###############################################
library(MASS)
model.rtree <- tree(damt ~ reg3 + reg4 + chld + lgif + rgif, data = data.train.std.y)
model.rtree
summary(model.rtree)
rtree <-summary(model.rtree)
mean((rtree$residuals)^2)
#1.906007 MSE Train 
cv.model.rtree =cv.tree(model.rtree)
plot(cv.model.rtree$size, cv.model.rtree$dev, type='b')
pred.valid.rtree <- predict(model.rtree, newdata = data.valid.std.y) # validation predictions
plot(pred.valid.rtree, y.valid)
mean((y.valid - pred.valid.rtree)^2) 
#2.241075 MSE Valid

########### Random Forest Bagged ######################################
library(randomForest)
set.seed(1)
model.bag1 = randomForest(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                            avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + 
                            tlag + agif, data = data.train.std.y, mtry=20, importance= TRUE)
model.bag1
summary(model.bag1)
mean((y.train - model.bag1$predicted)^2)
#1.49039 MSE Train 
importance(model.bag1)
varImpPlot(model.bag1)
pred.valid.bag1 <- predict(model.bag1, newdata = data.valid.std.y) # validation predictions
plot(pred.valid.bag1, y.valid)
mean((y.valid - pred.valid.bag1)^2) # mean prediction error
#1.704569 MSE Valid

######## Random forest ##########  (mtry = 5)
set.seed(1)
model.rfor <- randomForest(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                             avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + 
                             tlag + agif, data = data.train.std.y, mtry=5, importance= TRUE)
model.rfor
summary(model.rfor)
mean((y.train - model.rfor$predicted)^2)
#1.486771 MSE Train 
importance(model.rfor)
varImpPlot(model.rfor)
pred.valid.rfor <- predict(model.rfor, newdata = data.valid.std.y) # validation predictions
plot(pred.valid.rfor, y.valid)
mean((y.valid - pred.valid.rfor)^2) # mean prediction error
# 1.655456 MSE Valid

##################### Boosting #################
library(gbm)
set.seed(1)
model.gbm1 <- gbm(damt ~ reg2 + reg3 + reg4 + chld + hinc + wrat +
                   incm + plow + tgif + lgif + rgif + agif, 
                  data = data.train.std.y, distribution = "gaussian", 
                  n.trees=5000, interaction.depth=4, shrinkage=0.001, verbose=F)
model.gbm1
summary(model.gbm1)
pred.train.gbm1 <- predict(model.gbm1, newdata = data.train.std.y, n.trees=5000) # validation predictions
mean((y.train - pred.train.gbm1)^2) 
# 1.217787 MSE Train
pred.valid.gbm1 <- predict(model.gbm1, newdata = data.valid.std.y, n.trees=5000) # validation predictions
plot(pred.valid.gbm1, y.valid)
mean((y.valid - pred.valid.gbm1)^2) # mean prediction error
# 1.535696 MSE Valid


############ GAM ####################
library(gam)
model.gam = gam(damt ~ reg1 + reg2 + reg4 + home + G0_chld + G3_chld + s(hinc, 3) + s(wrat,5) + T_tgif + T_lgif + T_rgif + s(agif,2), data = data.train.std.y)
plot(model.gam, se=TRUE,col="blue")
model.gam
summary(model.gam)

mean((y.train - model.gam$fitted.values)^2)
pred.valid.gam <- predict(model.gam, newdata = data.valid.std.y)
plot(pred.valid.gam, y.valid)
mean((y.valid - pred.valid.gam)^2)


#########################################
# Results Prediction
#     train         valid
#      MSE        MSE        Model
#    1.35631  1.495806       ls1
#       -     1.569526       pca
#   1.906007   2.241075     rtree
#   1.49039    1.704569      bag1
#   1.486771   1.655456      rfor  
#   1.217787   1.535696       gbm
#   1.365967   1.48537       gam1

# selected model.ls1 since it is less complex as gam1
yhat.test <- predict(model.ls1, newdata = data.test.std) # test predictions

#########################################################################################################
# FINAL RESULTS

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="MKD.csv", row.names=FALSE) # use your initials for the file name

# submit the csv file in Angel for evaluation based on actual test donr and damt values
