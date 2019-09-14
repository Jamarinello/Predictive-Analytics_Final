# SET THE WORKING DIRECTORY AND SEED
setwd("E:/Documents Storage/Predictive Analytics/Final Project")
set.seed(1)

# LOAD LIBRARIES
library(rlist); library(plyr); library(ggplot2); library(cluster); library(lattice);
library(graphics); library(grid); library(gridExtra); library(ISLR); library(boot); library(caret)

# IMPORT THE DATA INTO A DATA FRAME
college = as.data.frame(read.csv("College.csv"))

college$Private<-factor(college$Private)
levels(college$Private) <- c("Public", "Private")

# TRIM THE DATAFRAME
collegeTrimed = college[,c(3:5,11,19)]

# CREATE NEW VARIABELS TO INVESTIGATE: Acceptance rate & Enrollment rate
Accept.Rate <- as.data.frame(round(collegeTrimed$Accept/collegeTrimed$Apps, 2))
names(Accept.Rate) <- "Accept.Rate"
Enroll.Rate <- as.data.frame(round(collegeTrimed$Enroll/collegeTrimed$Accept, 2))
names(Enroll.Rate) <- "Enroll.Rate"

# COMBINE DATA FRAMES
collegeCombined <- as.data.frame(c(collegeTrimed, Accept.Rate, Enroll.Rate))
#=============================================================================================================================================================================#
#=============================================================================================================================================================================#
# PLOT ACCEPTANCE RATE
Accept.Rate.Plot= ggplot(data=collegeCombined, aes(x=c(1:777), y=Accept.Rate)) + 
  geom_point() +
  geom_line(y=0.90, color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=0.40, color = "Red", size = 2, alpha = 0.5)
Accept.Rate.Plot

# IDENTIFY OUTLIERS THAT CAN HURT OUR MODEL
outliers.1 <- which(collegeCombined$Accept.Rate > 0.90)
outliers.2 <- which(collegeCombined$Accept.Rate < 0.40)
outliers.3 <- which(collegeCombined$Grad.Rate >= 100)
outliers <- c(outliers.1, outliers.2, outliers.3)
duplicated(outliers)

# identified that observations 17 & 251 duplicate in the outliers list.
outliers <- list.remove(outliers, c(93, 97))
collegeData <- collegeCombined[-outliers,]
public <- collegeCombined[-grep("Private", college$Private), TRUE,]
private <- collegeCombined[-grep("Public", college$Private), TRUE,]
#=============================================================================================================================================================================#
#============================================CREATING THE FIRST LINEAR REGRESSION MODEL=======================================================================================#
# DIVIDE THE DATASET
set <- createDataPartition(collegeData$Enroll, p = 2/3, list=F)
nrow(set)

nrow(collegeData)

trainData <- collegeData[set,]
testData <- collegeData[-set,]

# DEFINE OUR PREDICTIVE MODEL
model <- lm(Enroll~., data = trainData)
ControlParameters <- trainControl(method="repeatedcv",
                                  number = 10,
                                  repeats = 5)
modelLM <- train(Enroll~., data = trainData,
                 method = "glm",
                 trControl= ControlParameters)
#summary(modelLM)
#print(model)
print(modelLM)

# VALIDATE TRAINED MODEL
train.p1 <- predict(modelLM, trainData)
train.Error <- train.p1 - trainData$Enroll
head(train.p1)

head(train.Error)

sqrt(mean(train.Error^2))

# TEST THE MODEL
test.p1 <- predict(modelLM, testData)
test.Error <- test.p1 - testData$Enroll
head(test.p1)

head(test.Error)

sqrt(mean(test.Error^2))

#=============================================================================================================================================================================#
# APPLY THE FIRST MODEL TO THE ENTIRE "collegeData" DATASET
collegeData.Prediction <- predict(modelLM, collegeData)
cD.FullError1 <- round(collegeData.Prediction - collegeData$Enroll, 0)
collegeData.model.RMSE <- sqrt(mean(cD.FullError1^2))

# EVALUATE ERROR VALUES
cD.ErrorValues1 <- as.data.frame(cD.FullError1)
model1error.collegeData <- summary(cD.ErrorValues1)
which(cD.ErrorValues1 == max(cD.ErrorValues1))
## [1] 415
which(cD.ErrorValues1 == min(cD.ErrorValues1))
## [1] 501
error.Plot= ggplot(data=cD.ErrorValues1, aes(x=1:676, y=cD.FullError1)) + 
  geom_point() +
  geom_line(y= min(cD.ErrorValues1), color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=max(cD.ErrorValues1), color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=-100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=500, color = "Yellow", size = 1, alpha = 0.5) +
  geom_line(y=-500, color = "Yellow", size = 1, alpha = 0.5)
error.Plot

hist1 <- qplot(x = cD.FullError1, data = cD.ErrorValues1, geom = "histogram", xlim = c(-500, 500), binwidth = 10, xlab = "Prediction Error", main = "collegeData: Model 1" )
#=============================================================================================================================================================================#
# IDENTIFY EXTREME OUTLIERS IN "collegeData"
#collegeData[415,]
#collegeData[501,]
# IDENTIFY MODERATE OUTLIERS IN "collegeData"
moderate.Outlier.1 <- which(cD.ErrorValues1 > 500)
moderate.Outlier.2 <- which(cD.ErrorValues1 < -500)
moderate.Outliers <- c(moderate.Outlier.1, moderate.Outlier.2)
#collegeData[moderate.Outliers,]
# PRUNING FOR A MORE ACCURATE MODEL #
collegeData2 <- collegeData[-c(moderate.Outliers),]
#=============================================================================================================================================================================#
#===========================================CREATING THE SECOND LINEAR REGRESSION MODEL=======================================================================================#
# DIVIDE THE DATASET
set2 <- createDataPartition(collegeData2$Enroll, p = 2/3, list=F)
nrow(set2)
nrow(collegeData2)

trainData2 <- collegeData2[set2,]
testData2 <- collegeData2[-set2,]
# DEFINE OUR PREDICTIVE MODEL
model.2 <- lm(Enroll~., data = trainData2)
modelLM.2 <- train(Enroll~., data = trainData2,
                   method = "glm",
                   trControl= ControlParameters)
#summary(modelLM.2)
#print(model.2)
print(modelLM.2)

# VALIDATE TRAINED MODEL
train.p2 <- predict(modelLM.2, trainData2)
train.Error2 <- train.p2 - trainData2$Enroll
head(train.p2)

head(train.Error2)

sqrt(mean(train.Error2^2))

# TEST THE MODEL
test.p2 <- predict(modelLM.2, testData2)
test.Error2 <- test.p2 - testData2$Enroll
head(test.p2)

head(test.Error2)

sqrt(mean(test.Error2^2))

#=============================================================================================================================================================================#
# APPLY THE SECOND MODEL TO THE ENTIRE "collegeData" DATASET
collegeData.Prediction2 <- predict(modelLM.2, collegeData)
cD.FullError2 <- round(collegeData.Prediction2 - collegeData$Enroll, 0)
collegeData.model2.RMSE <- sqrt(mean(cD.FullError2^2))

# EVALUATE "collegeData" ERROR VALUES
cD.ErrorValues2 <- as.data.frame(cD.FullError2)
model2error.collegeData <- summary(cD.ErrorValues2)
which(cD.ErrorValues2 == max(cD.ErrorValues2))

which(cD.ErrorValues2 == min(cD.ErrorValues2))

error.Plot2= ggplot(data=cD.ErrorValues2, aes(x=1:676, y=cD.FullError2)) + 
  geom_point() +
  geom_line(y= min(cD.ErrorValues2), color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=max(cD.ErrorValues2), color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=-100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=500, color = "Yellow", size = 1, alpha = 0.5) +
  geom_line(y=-500, color = "Yellow", size = 1, alpha = 0.5)
error.Plot2

hist2 <- qplot(x = cD.FullError2, data = cD.ErrorValues2, geom = "histogram", xlim = c(-500, 500), binwidth = 10, xlab = "Prediction Error", main = "collegeData: Model 2" )
#=============================================================================================================================================================================#
# APPLY THE FIRST MODEL TO THE ENTIRE "collegeData2" DATASET
collegeData2.Prediction <- predict(modelLM, collegeData2)
cD2.FullError3 <- round(collegeData2.Prediction - collegeData2$Enroll, 0)
collegeData2.model1.RMSE <- sqrt(mean(cD2.FullError3^2))

# EVALUATE "collegeData2" ERROR VALUES
cD2.ErrorValues3 <- as.data.frame(cD2.FullError3)
model1error.collegeData2 <- summary(cD2.ErrorValues3)
which(cD2.FullError3 == max(cD2.FullError3))

which(cD2.FullError3 == min(cD2.FullError3))

error.Plot3= ggplot(data=cD2.ErrorValues3, aes(x=1:634, y=cD2.FullError3)) + 
  geom_point() +
  geom_line(y= min(cD2.FullError3), color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=max(cD2.FullError3), color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=-100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=500, color = "Yellow", size = 1, alpha = 0.5) +
  geom_line(y=-500, color = "Yellow", size = 1, alpha = 0.5)
error.Plot3

hist3 <- qplot(x = cD2.FullError3, data = cD2.ErrorValues3, geom = "histogram", xlim = c(-500, 500), binwidth = 10, xlab = "Prediction Error", main = "collegeData2: Model 1" )
#=============================================================================================================================================================================#
# APPLY THE SECOND MODEL TO THE ENTIRE "collegeData2" DATASET
collegeData2.Prediction2 <- predict(modelLM.2, collegeData2)
cD2.FullError4 <- round(collegeData2.Prediction2 - collegeData2$Enroll, 0)
collegeData2.model2.RMSE <- sqrt(mean(cD2.FullError4^2))

# EVALUATE "collegeData2" ERROR VALUES
cD2.ErrorValues4 <- as.data.frame(cD2.FullError4)
model2error.collegeData2 <- summary(cD2.ErrorValues4)
which(cD2.FullError4 == max(cD2.FullError4))

which(cD2.FullError4 == min(cD2.FullError4))

error.Plot4= ggplot(data=cD2.ErrorValues4, aes(x=1:634, y=cD2.FullError4)) + 
  geom_point() +
  geom_line(y= min(cD2.FullError4), color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=max(cD2.FullError4), color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=-100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=500, color = "Yellow", size = 1, alpha = 0.5) +
  geom_line(y=-500, color = "Yellow", size = 1, alpha = 0.5)
error.Plot4

hist4 <- qplot(x = cD2.FullError4, data = cD2.ErrorValues4, geom = "histogram", xlim = c(-500, 500), binwidth = 10, xlab = "Prediction Error", main = "collegeData2: Model 2")
#=============================================================================================================================================================================#
# APPLY THE FIRST AND SECOND MODEL TO THE "private" DATASET
private.Prediction <- predict(modelLM, private)
priv.FullError1 <- round(private.Prediction - private$Enroll, 0)
private.model1.RMSE <- sqrt(mean(priv.FullError1^2))
priv.ErrorValues1 <- as.data.frame(priv.FullError1)
model1error.private <- summary(priv.ErrorValues1)

private.Prediction2 <- predict(modelLM.2, private)
priv.FullError2 <- round(private.Prediction2 - private$Enroll, 0)
private.model2.RMSE <- sqrt(mean(priv.FullError2^2))
priv.ErrorValues2 <- as.data.frame(priv.FullError2)
model2error.private <- summary(priv.ErrorValues2)
hist5 <- qplot(x = priv.FullError1, data = priv.ErrorValues1, geom = "histogram", xlim = c(-500, 500), binwidth = 10, xlab = "Prediction Error", main = "private: Model 1")
hist6 <- qplot(x = priv.FullError2, data = priv.ErrorValues2, geom = "histogram", xlim = c(-500, 500), binwidth = 10, xlab = "Prediction Error", main = "private: Model 2")
#=============================================================================================================================================================================#
# APPLY THE FIRST AND SECOND MODEL TO THE "public" DATASET
public.Prediction <- predict(modelLM, public)
pub.FullError1 <- round(public.Prediction - public$Enroll, 0)
public.model1.RMSE <- sqrt(mean(pub.FullError1^2))
pub.ErrorValues1 <- as.data.frame(pub.FullError1)
model1error.public <- summary(pub.ErrorValues1)

public.Prediction2 <- predict(modelLM.2, public)
pub.FullError2 <- round(public.Prediction2 - public$Enroll, 0)
public.model2.RMSE <- sqrt(mean(pub.FullError2^2))
pub.ErrorValues2 <- as.data.frame(pub.FullError2)
model2error.public <- summary(pub.ErrorValues2)
hist7 <- qplot(x = pub.FullError1, data = pub.ErrorValues1, geom = "histogram", xlim = c(-500, 500), binwidth = 10, xlab = "Prediction Error", main = "public: Model 1")
hist8 <- qplot(x = pub.FullError2, data = pub.ErrorValues2, geom = "histogram", xlim = c(-500, 500), binwidth = 10, xlab = "Prediction Error", main = "public: Model 2")
#=============================================================================================================================================================================#
# EVALUATE RESULTS
model1error.collegeData

model1error.collegeData2

model2error.collegeData

model2error.collegeData2

hist1
hist2
hist3
hist4

model1error.private

model2error.private

model1error.public

model2error.public

hist5
hist6
hist7
hist8

RMSE.list <- as.data.frame(c(collegeData.model.RMSE, collegeData.model2.RMSE,
                             collegeData2.model1.RMSE, collegeData2.model2.RMSE,
                             private.model1.RMSE, private.model2.RMSE,
                             public.model1.RMSE, public.model2.RMSE))
colnames(RMSE.list) <- c("RMSE Values")
row.names(RMSE.list) <- c("collegeData.model1", "collegeData.model2",
                          "collegeData2.model1", "collegeData2.model2",
                          "private.model1", "private.model2",
                          "public.model1", "public.model2")
RMSE.list
