# SET THE WORKING DIRECTORY
setwd("E:/Documents Storage/Predictive Analytics/Final Project")

# SET THE SEED
set.seed(1)

# LOAD LIBRARIES
library(rlist)
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(graphics)
library(grid)
library(gridExtra)
library(ISLR)
library(boot)
library(caret)


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
#=================================================================================================================================#




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

summary(modelLM)
print(model)
print(modelLM)


# TRAIN THE MODEL
train.p1 <- predict(modelLM, trainData)
head(train.p1)

train.Error <- train.p1 - trainData$Enroll
head(train.Error)

sqrt(mean(train.Error^2))


# TEST THE MODEL
test.p1 <- predict(modelLM, testData)
head(test.p1)

test.Error <- test.p1 - testData$Enroll
head(test.Error)

sqrt(mean(test.Error^2))


# APPLY THE MODEL TO THE ENTIRE DATASET
full.p1 <- predict(modelLM, collegeData)
head(full.p1)

full.Error <- round(full.p1 - collegeData$Enroll, 0)
head(full.Error)

sqrt(mean(full.Error^2))

errorValues1 <- as.data.frame(full.Error)
head(errorValues1)

#=================================================================================================================================#




# IDENTIFY MAX AND MIN OUTLIERS
max(errorValues1)
which(errorValues1 == 2913)

min(errorValues1)
which(errorValues1 == -2279)

# PLOT ERROR VALUES
error.Plot= ggplot(data=errorValues1, aes(x=1:676, y=full.Error)) + 
  geom_point() +
  geom_line(y= -2279, color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=2913, color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=-100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=500, color = "Yellow", size = 1, alpha = 0.5) +
  geom_line(y=-500, color = "Yellow", size = 1, alpha = 0.5)

error.Plot


# IDENTIFY EXTREME OUTLIERS IN "collegeData"
collegeData[415,]
collegeData[501,]


# IDENTIFY MODERATE OUTLIERS IN "collegeData"
moderate.Outlier.1 <- which(errorValues1 > 500)
moderate.Outlier.2 <- which(errorValues1 < -500)
moderate.Outliers <- c(moderate.Outlier.1, moderate.Outlier.2)

collegeData[moderate.Outlier.1,]
collegeData[moderate.Outlier.2,]
#=================================================================================================================================#
#=================================================================================================================================#
#=================================================================================================================================#




# PRUNING FOR A MORE ACCURATE MODEL #
collegeData2 <- collegeData[-c(moderate.Outliers),]

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

summary(modelLM.2)
print(model.2)
print(modelLM.2)


# TRAIN THE MODEL
train.p2 <- predict(modelLM.2, trainData2)
head(train.p2)

train.Error2 <- train.p2 - trainData2$Enroll
head(train.Error2)

sqrt(mean(train.Error2^2))


# TEST THE MODEL
test.p2 <- predict(modelLM.2, testData2)
head(test.p2)

test.Error2 <- test.p2 - testData2$Enroll
head(test.Error2)

sqrt(mean(test.Error2^2))


# APPLY THE MODEL TO THE ENTIRE DATASET
Full.p2 <- predict(modelLM.2, collegeData2)
head(Full.p2)

full.Error2 <- round(Full.p2 - collegeData2$Enroll, 0)
head(full.Error2)

sqrt(mean(full.Error2^2))

errorValues2 <- as.data.frame(full.Error2)
head(errorValues2)
#=================================================================================================================================#




# IDENTIFY MAX AND MIN OUTLIERS
max(errorValues2)
which(errorValues2 == 624)

min(errorValues2)
which(errorValues2 == -575)

# PLOT ERROR VALUES
error.Plot2= ggplot(data=errorValues2, aes(x=1:634, y=full.Error2)) + 
  geom_point() +
  geom_line(y= -575, color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=624, color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=-100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=500, color = "Yellow", size = 1, alpha = 0.5) +
  geom_line(y=-500, color = "Yellow", size = 1, alpha = 0.5)

error.Plot2


# IDENTIFY EXTREME OUTLIERS IN "collegeData"
collegeData[50,]
collegeData[536,]
#=================================================================================================================================#
#=================================================================================================================================#
#=================================================================================================================================#




# APPLY THE MODEL TO THE ENTIRE DATASET
Full.p3 <- predict(modelLM.2, collegeData)
head(Full.p3)

full.Error3 <- round(Full.p3 - collegeData$Enroll, 0)
head(full.Error3)

sqrt(mean(full.Error3^2))

errorValues3 <- as.data.frame(full.Error3)
head(errorValues3)

max(errorValues3)
which(errorValues3 == 3798)

min(errorValues3)
which(errorValues3 == -2162)
# PLOT ERROR VALUES
error.Plot3= ggplot(data=errorValues3, aes(x=1:676, y=full.Error3)) + 
  geom_point() +
  geom_line(y= -2162, color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=3798, color = "Red", size = 2, alpha = 0.5) +
  geom_line(y=100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=-100, color = "Green", size = 1, alpha = 0.5) +
  geom_line(y=500, color = "Yellow", size = 1, alpha = 0.5) +
  geom_line(y=-500, color = "Yellow", size = 1, alpha = 0.5)

error.Plot3

