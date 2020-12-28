######################### Question 1 #########################
##############################################################
# The goal is to randomly split the entire data set into training, validation, and test samples using only single family homes and then using a 50%/25%/25% split

# Import file
file_path <- "C:\\Users\\seanj\\Desktop\\Machine Learning I\\HW1\\GradedHW1-All-Data.csv"

AllData <- read.table(file_path,
                      header=T,
                      sep=",",
                      stringsAsFactors = F,
                      na.strings="")

# First remove all observations except those that are single family homes.
AllData <- AllData[AllData$Bldg.Type=="1Fam",]
str(AllData)
class(AllData)
# Next, randomly draw the training sample. If the training sample fraction does not result in an integer number of observations, round up
RPerm <- sample(nrow(AllData))
AllData <- AllData[RPerm,]
TrainInd <- ceiling(nrow(AllData)/2)

# From the remaining observations, randomly draw the validation sample. Again rounding up if the split fraction does not result in an integer value
ValInd <- ceiling((nrow(AllData)-TrainInd)/2)+TrainInd

# Finally, use the remaining observations for the test sample.
TrainData <- AllData[1:TrainInd,]
ValData <- AllData[(TrainInd+1):ValInd,]
TestData <- AllData[(ValInd+1):nrow(AllData),]

# Confirm the number of data by checking the dimensions
dim(TrainData) # 1212 rows, 79 columns
dim(ValData) # 606 rows, 79 columns
dim(TestData) # 606 rows, 79 columns



######################### Question 2 #########################
##############################################################
# Without standardizing (or transforming) the variables, fit a k-NN regressions to the data for k =1,2, ,40
# Make plot of the MSE calculated from the validation data against k. 
# Answer the questions on the Google form. Turn in the plot of the MSE vs. k as instructed below.

###############################################################

# Import three dataset
file_path_1 <- "C:\\Users\\seanj\\Desktop\\Machine Learning I\\HW1\\GradedHW1-Train-Data.csv"
train <- read.table(file_path_1,
                      header=T,
                      sep=",",
                      stringsAsFactors = F,
                      na.strings="")
train <- train[train$Bldg.Type=="1Fam",]

file_path_2 <- "C:\\Users\\seanj\\Desktop\\Machine Learning I\\HW1\\GradedHW1-Validation-Data.csv"
val <- read.table(file_path_2,
                      header=T,
                      sep=",",
                      stringsAsFactors = F,
                      na.strings="")
val <- val[val$Bldg.Type=="1Fam",]

file_path_3 <- "C:\\Users\\seanj\\Desktop\\Machine Learning I\\HW1\\GradedHW1-Test-Data.csv"
test <- read.table(file_path_3,
                      header=T,
                      sep=",",
                      stringsAsFactors = F,
                      na.strings="")
test <- test[test$Bldg.Type=="1Fam",]

#############################################################################
# Assign X variables for Training Dataset
t1.lotarea <- train$Lot.Area
t2.wt <- train$Total.Bsmt.SF
t3.livarea <- train$Gr.Liv.Area
t4.fullbath <- train$Full.Bath
t5.bedroom <- train$Bedroom.AbvGr

t.yearbuilt <- train$Year.Built
year = 2010
t6.buildage <- year - t.yearbuilt
print(t6.buildage)

ty.sp <- train$SalePrice

# Assign X variables for Validation Dataset
v1.lotarea <- val$Lot.Area
v2.wt <- val$Total.Bsmt.SF
v3.livarea <- val$Gr.Liv.Area
v4.fullbath <- val$Full.Bath
v5.bedroom <- val$Bedroom.AbvGr

length(val$Lot.Area)
dim(val)
# Create Building Age variable
v.yearbuilt <- val$Year.Built
year = 2010
v6.buildage <- year - v.yearbuilt
print(v6.buildage)

# Set Target Variable
vy.sp <- val$SalePrice

# Plot
plot(v1.lotarea, vy.sp)
plot(v2.wt, vy.sp) # Somewhat positive correlation
plot(v3.livarea, vy.sp) # Strongly positive correlation
plot(v4.fullbath, vy.sp)
plot(v5.bedroom, vy.sp)
plot(v6.buildage, vy.sp) # Somewhat exponential decay

# Add a least-squares regression fit for the hell of it. --------------------------------------------------------
out <- lm(ty.sp ~ t1.lotarea + t2.wt + t3.livarea + t4.fullbath + t5.bedroom)
summary(out) # wt, livarea , fullbath, bedrooms are statistically significant
names(out)

# knn regression ---------------------------------------------------------------

# Now do a knn regression fit with prediction done on a grid to show the fitted surface.
# Need package FNN to get the function knn.reg().

suppressWarnings(if(!require("FNN")) { install.packages("FNN"); require("FNN") })

# Train DataFrame
TrainDF <- data.frame(la=t1.lotarea, wt=t2.wt, liv=t3.livarea, fb=t4.fullbath, bed=t5.bedroom,age=t6.buildage, yval=ty.sp)
TrainDF <- TrainDF[complete.cases(TrainDF), ]
  # Check the Train Data
  class(TrainDF)
  str(TrainDF)
  dim(TrainDF)
  sum(is.na(TrainDF))
  TrainDF

# Validation DataFrame
ValDF <- data.frame(la=v1.lotarea, wt=v2.wt, liv=v3.livarea, fb=v4.fullbath, bed=v5.bedroom,age=v6.buildage, yval=vy.sp)
ValDF <- ValDF[complete.cases(ValDF), ]
# Check the Validation Data
class(ValDF)
str(ValDF)
dim(ValDF)
sum(is.na(ValDF))
ValDF
  
# Assign X variables for Test Dataset
p1.lotarea <- test$Lot.Area
p2.wt <- test$Total.Bsmt.SF
p3.livarea <- test$Gr.Liv.Area
p4.fullbath <- test$Full.Bath
p5.bedroom <- test$Bedroom.AbvGr
  
# Create Building Age variable
p.yearbuilt <- test$Year.Built
year = 2010
p6.buildage <- year - p.yearbuilt
print(v6.buildage)
  
# Set Target Variable
py.sp <- test$SalePrice
  
# I could have used the R function scale() instead.
TestDF <- data.frame(la=p1.lotarea, wt=p2.wt, liv=p3.livarea, fb=p4.fullbath, bed=p5.bedroom, age=p6.buildage, yval=py.sp)
TestDF <- TestDF[complete.cases(TestDF), ]
  

# Question 2
RMSE <- rep(NA,40)
for (k in 1:40){
out <- knn.reg(train=TrainDF[,c(1:6)],
               test=ValDF[,c(1:6)], 
               y=ty.sp, 
               k=k) 
ypred <- out$pred
RMSE[k] <- mean((ValDF$yval-ypred)^2)^0.5
}
plot(RMSE, main="Question 2 by Sean Jung\n (Not Standardized)", xlab="k")
which.min(RMSE)
RMSE[12]
RMSE[1]
RMSE[20]

# Question 3
out <- knn.reg(train=TrainDF[,c(1:6)],
               test=TestDF[,c(1:6)], 
               y=ty.sp, 
               k=12) 
ypred <- out$pred
mean((TestDF$yval-ypred)^2)^0.5 #58,539 when k = 12


# Re-Scaling (standardization) -----------------------------------------------------------

#----------------------------------------------------------------
# Training
TrainDF$la_r <- (TrainDF$la-mean(t1.lotarea))/sd(t1.lotarea)
TrainDF$wt_r <- (TrainDF$wt-mean(t2.wt))/sd(t2.wt)
TrainDF$liv_r <- (TrainDF$liv-mean(t3.livarea))/sd(t3.livarea)
TrainDF$fb_r <- (TrainDF$fb-mean(t4.fullbath))/sd(t4.fullbath)
TrainDF$bed_r <- (TrainDF$bed-mean(t5.bedroom))/sd(t5.bedroom)
TrainDF$age_r <- (TrainDF$age-mean(t6.buildage))/sd(t6.buildage)
ty.sp_r <- (TrainData$SalePrice-mean(TrainData$SalePrice))/sd(TrainData$SalePrice)

# Validation
ValDF$la_r <- (ValDF$la-mean(t1.lotarea))/sd(t1.lotarea)
ValDF$wt_r <- (ValDF$wt-mean(t2.wt))/sd(t2.wt)
ValDF$liv_r <- (ValDF$liv-mean(t3.livarea))/sd(t3.livarea)
ValDF$fb_r <- (ValDF$fb-mean(t4.fullbath))/sd(t4.fullbath)
ValDF$bed_r <- (ValDF$bed-mean(t5.bedroom))/sd(t5.bedroom)
ValDF$age_r <- (ValDF$age-mean(t6.buildage))/sd(t6.buildage)

# Test
TestDF$la_r <- (TestDF$la-mean(t1.lotarea))/sd(t1.lotarea)
TestDF$wt_r <- (TestDF$wt-mean(t2.wt))/sd(t2.wt)
TestDF$liv_r <- (TestDF$liv-mean(t3.livarea))/sd(t3.livarea)
TestDF$fb_r <- (TestDF$fb-mean(t4.fullbath))/sd(t4.fullbath)
TestDF$bed_r <- (TestDF$bed-mean(t5.bedroom))/sd(t5.bedroom)
TestDF$age_r <- (TestDF$age-mean(t6.buildage))/sd(t6.buildage)

# Question 4
RMSE <- rep(NA,40)
for (k in 1:40){
out <- knn.reg(train=TrainDF[,c(8:13)],
               test=ValDF[,c(8:13)],
               y=ty.sp,
               k=k)
ypred <- out$pred
RMSE[k] <- mean((ValDF$yval-ypred)^2)^0.5
}
plot(RMSE, main="Question 4 by Sean Jung\n (Standardized)", xlab="k")
which.min(RMSE)
RMSE[12]
RMSE[1]
RMSE[20]

# Question 5
out <- knn.reg(train=TrainDF[,c(8:13)],
               test=TestDF[,c(8:13)],
               y=ty.sp,
               k=12)
ypred <- out$pred
mean((TestDF$yval-ypred)^2)^0.5
