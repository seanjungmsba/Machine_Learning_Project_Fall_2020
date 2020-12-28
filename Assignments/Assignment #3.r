setwd("C:/Users/seanj/Desktop/Machine Learning I/HW3")
set.seed(20201116)
DataOrig <- read.table("spambasedata-Orig.csv",
                       sep=",",
                       header=T,
                       stringsAsFactors=F)

ord <- sample(nrow(DataOrig))
DataOrig <- DataOrig[ord,]

# Change IsSpam to a factor
DataOrig$IsSpam <- factor(DataOrig$IsSpam)
DataOrig$IsSpam <- as.numeric(levels(DataOrig$IsSpam)[DataOrig$IsSpam])

# Doing a 60-20-20 split
TrainInd <- ceiling(nrow(DataOrig)*0.6)
TrainDF <- DataOrig[1:TrainInd,]
tmpDF <- DataOrig[-(1:TrainInd),]
ValInd <- ceiling(nrow(tmpDF)*0.5)
ValDF <- tmpDF[1:ValInd,]
TestDF <- tmpDF[-(1:ValInd),]

remove(TrainInd,tmpDF,ValInd,ord)

# Question 1 --------------------------------------------------------------
# Stepwise Logistic Regression

# I am setting up the formulas for you. Nonetheless, you should examine how the BigFM is created

SmallFm <- IsSpam ~ 1 # Fit only the intercept
Vars <- names(TrainDF) 

BigFm <- paste(Vars[58],"~",paste(Vars[1:57],collapse=" + "),sep=" ") # Fit all the variables
BigFm <- formula(BigFm)

# Your code to do stepwise logistic regression and compute the predicted probabilities for the validation and test data goes here.

OutSmall <- glm(SmallFm,family=binomial,data=TrainDF) 
summary(OutSmall) # AIC: 3704.7
                  # Number of Fisher Scoring iterations: 4


sc <- list(lower=SmallFm, upper=BigFm)
out <- step(OutSmall, scope = sc, direction="both") #scope argument is required; 
# direction = "both" -> opens possibility of dropping variables

summary(out)
# Null deviance: 3702.7  on 2760  degrees of freedom
# Residual deviance: 1086.5  on 2718  degrees of freedom
# AIC: 1172.5
# Number of Fisher Scoring iterations: 13

# View(out$coefficients) to examine the features in the final stepwise function

AIC(out)
# 1172.462

# Make Predictions
ValPred <- predict.glm(out,ValDF,type = 'response')
TestPred <- predict.glm(out,TestDF, type = 'response')

#save(DataComp,file="DataComp.Rdata") -> you can save when the data is huge and want to save the progress

#ROC curve 
ROCPlot <- function(valpred,out,Plot=T,Add=F) {
  NHam <- sum(out==0)
  NSpam <- sum(out==1)
  valpredS <- unique(sort(valpred))
  x <- rep(NA,length(valpredS))
  y <- rep(NA,length(valpredS))
  for(i in 1:length(valpredS)) {
    x[i] <- sum(valpred>=valpredS[i]&out==0)/NHam
    y[i] <- sum(valpred>=valpredS[i]&out==1)/NSpam
  }
  x <- c(0,x,1)
  y <- c(0,y,1)
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  
  AUC <- sum((x[2:length(x)]-x[1:(length(x)-1)])*(y[2:length(y)]+y[1:(length(y)-1)])/2)
  
  if(Add) {
    plot(x,y,type="l",xlim=c(0,1),ylim=c(0,1),xlab="P( classified + | Is - )",ylab="P( classified + | Is + )")
    title("ROC Curve by Sean Jung")
    mtext(paste("AUC =",round(AUC,3)),side=3,line=0.5)
    abline(0,1)
    par(pty="m")
  } else {
    if(Plot) {
      par(pty="s")
      plot(x,y,type="l",xlim=c(0,1),ylim=c(0,1),xlab="P( classified + | Is - )",ylab="P( classified + | Is + )")
      title("ROC Curve by Sean Jung")
      mtext(paste("AUC =",round(AUC,3)),side=3,line=0.5)
      abline(0,1)
      par(pty="m")
    }
  }
  invisible(list(x=c(0,x,1),y=c(0,y,1),AUC=AUC))
}
##################

#source("RocPlot.r")
ROCPlot(ValPred,ValDF$IsSpam)
ROCPlot(TestPred,TestDF$IsSpam)

# Question 2 --------------------------------------------------------------

if(!require("randomForest")) { install.packages("randomForest"); require("randomForest") }

# Your code to compute the random forest and compute the predicted probabilities for the validation and test data goes here.

out2 <- randomForest(BigFm,data=TrainDF, ntree=1000)

# mtry: Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)

# nodesize: Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). Note that the default values are different for classification (1) and regression (5)

out2$mtry #19
out2$nodesize # NULL


RandValPred <- predict(out2,newdata=ValDF,type='prob')
RandValPred <- RandValPred[,2]

RandTestPred <- predict(out2,newdata=TestDF,type='prob')
RandTestPred <- RandTestPred[,2]

#source("RocPlot.r")
ROCPlot(RandValPred,ValDF$IsSpam)
ROCPlot(RandTestPred,TestDF$IsSpam)

# Question 3 Wide --------------------------------------------------------------

# Write out the data for the neural net models
#write.table(TrainDF,file="HWTrain.csv",sep=",",row.names=F,col.names=T)
#write.table(ValDF,file="HWVal.csv",sep=",",row.names=F,col.names=T)
#write.table(TestDF,file="HWTest.csv",sep=",",row.names=F,col.names=T)

# -------------------------------------------------------------------------

# Read in the neural net output and compute the AUC for the validation data.
SpamNNWideTrainOutput <- read.table("SpamNNWideTrainDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideValOutput <- read.table("SpamNNWideValDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideTestOutput <- read.table("SpamNNWideTestDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

names(SpamNNWideValOutput)
ROCPlot(SpamNNWideValOutput$ValP,SpamNNWideValOutput$IsSpam)

# Question 3 Deep --------------------------------------------------------------
# 
write.table(TrainDF,file="HWTrain.csv",sep=",",row.names=F,col.names=T)
write.table(ValDF,file="HWVal.csv",sep=",",row.names=F,col.names=T)
write.table(TestDF,file="HWTest.csv",sep=",",row.names=F,col.names=T)

# -------------------------------------------------------------------------

# Read in the neural net output and compute the AUC for the validation data.

SpamNNDeepTrainOutput <- read.table("SpamNNDeepTrainDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNDeepValOutput <- read.table("SpamNNDeepValDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNDeepTestOutput <- read.table("SpamNNDeepTestDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

names(SpamNNDeepValOutput)
ROCPlot(SpamNNDeepValOutput$ValP,SpamNNDeepValOutput$IsSpam)
