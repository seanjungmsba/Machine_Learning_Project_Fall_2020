############################## Data Preparation ############################## 
# read the data
library(data.table)
library(dplyr)
library(fastDummies)

memory.size(max = 1e10)
memory.limit(size = 1e10)
Train <- fread("ProjectTrainingData.csv")
#Train <- data.frame(Train)

# take a look at total column names
colnames(Train)

# remove id, device_id, device_ip
Train[, id := NULL] 
Train[, device_id := NULL] 
Train[, device_ip := NULL] 
Train[, site_id := NULL]
Train[, site_domain := NULL]
Train[, app_id := NULL] 
Train[, app_domain := NULL] 

# break up hour into time category for hour of day and date for day
Train[, time := hour %% 100]
Train[, Morning := 0]; Train[Train$time >= 5 & Train$time < 11, Day := 1]
Train[, Day := 0]; Train[Train$time >= 11 & Train$time < 17, Day := 1]
Train[, Evening := 0]; Train[Train$time >= 17 & Train$time < 20, Evening := 1]
Train[, Night := 0]; Train[Train$time >= 20 & Train$time < 24, Night := 1];Train[Train$time == 0, Night := 1]

Train[, time := NULL]

Train[, date := (hour %% 10000 - hour %% 100)/100]
Train[, Tuesday := 0]; Train[Train$date == 21|Train$date == 28, Tuesday := 1]
Train[, Monday := 0]; Train[Train$date == 27, Monday := 1]
Train[, Wednesday := 0]; Train[Train$date == 22, Wednesday := 1]
Train[, Thursday := 0]; Train[Train$date == 23, Thursday := 1]
Train[, Saturday := 0]; Train[Train$date == 25, Saturday := 1]
Train[, Sunday := 0]; Train[Train$date == 26, Sunday := 1]

Train[, date := NULL]
Train[, hour := NULL]

# Translate -----------------------------------------------------------
Train_matrix <- as.matrix(Train)

# set up variables
y = as.integer(Train_matrix[, 'click'])
Tables = list()

# Translate C1 -----------------------------------------------------------

x = Train_matrix[,'C1']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]

Labs = names(out)

TransTable_C1 = c(rep("more_freq", 1), rep("less_freq", 3), rep("least_freq", 3))
names(TransTable_C1) = Labs

# translation table
Tables[[1]] = TransTable_C1

# apply the table to original column and deal with NAs
Train$C1 = TransTable_C1[x]
#Train$C1[is.na(Train$C1)] = "less_freq"

# Translate banner_pos -----------------------------------------------------------

x = Train_matrix[,'banner_pos']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]

Labs = names(out)

TransTable_banner_pos = c(rep("more_freq", 1), rep("less_freq", 4), rep("least_freq", 2))
names(TransTable_banner_pos) = Labs

# translation table
Tables[[2]] = TransTable_banner_pos

# apply the table to original column and deal with NAs
Train$banner_pos = TransTable_banner_pos[x]
#Train$banner_pos[is.na(Train$banner_pos)] = "less_freq"

# Translate site_category -----------------------------------------------------------

x = Train_matrix[,'site_category']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]

Labs = names(out)

TransTable_site_category = c(rep("more_freq", 1), rep("less_freq", 9), rep("least_freq", length(Labs)-10))
names(TransTable_site_category) = Labs

# translation table
Tables[[3]] = TransTable_site_category

# apply the table to original column and deal with NAs
Train$site_category = TransTable_site_category[x]
#Train$site_category[is.na(Train$site_category)] = "less_freq"

# Translate app_category --------------------------------------------------
x = Train_matrix[,'app_category']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]

Labs = names(out)

TransTable_app_category = c(rep("more_freq", 5), rep("less_freq", 17), rep("least_freq", 14))
names(TransTable_app_category) = Labs

# translation table
Tables[[4]] = TransTable_app_category

# apply the table to original column and deal with NAs
Train$app_category = TransTable_app_category[x]
#Train$app_category[is.na(Train$app_category)] = "less_freq"

# Translate_device_model --------------------------------------------------
x = Train_matrix[,'device_model']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]

Labs = names(out)

TransTable_device_model = c(rep("more_freq", 161), rep("less_freq", 312), rep("least_freq", length(Labs)-473))
names(TransTable_device_model) = Labs

# translation table
Tables[[5]] = TransTable_device_model

# apply the table to original column and deal with NAs
Train$device_model = TransTable_device_model[x]
#Train$device_model[is.na(Train$device_model)] = "less_freq"

# Translate_device_type ---------------------------------------------------
x = Train_matrix[,'device_type']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]

Labs = names(out)

TransTable_device_type = c(rep("more_freq", 2), rep("less_freq", 2), rep("least_freq", 1))
names(TransTable_device_type) = Labs

# translation table
Tables[[6]] = TransTable_device_type

# apply the table to original column and deal with NAs
Train$device_type = TransTable_device_type[x]
#Train$device_type[is.na(Train$device_type)] = "less_freq"

# Translate_device_conn_type ----------------------------------------------
x = Train_matrix[,'device_conn_type']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]

Labs = names(out)

TransTable_device_conn_type = c(rep("more_freq", 1), rep("less_freq", 1), rep("least_freq", 2))
names(TransTable_device_conn_type) = Labs

# translation table
Tables[[7]] = TransTable_device_conn_type

# apply the table to original column and deal with NAs
Train$device_conn_type = TransTable_device_conn_type[x]
#Train$device_conn_type[is.na(Train$device_conn_type)] = "less_freq"

# Translate C14 -----------------------------------------------------------
x = Train_matrix[,'C14']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]
Labs = names(out)

# out is evenly distributed from 1 to 0, so we will use the criteria as follows:
sum(out >= 0.75) # 15
sum(out > 0.25 & out < 0.75) #340
sum(out <= 0.25) # 2110


TransTable_C14 = c(rep("more_freq", 15), rep("less_freq", 340), rep("least_freq", 2110))
names(TransTable_C14) = Labs

# translation table
Tables[[8]] = TransTable_C14

# apply the table to original column and deal with NAs
Train$C14 = TransTable_C14[x]
#Train$C14[is.na(Train$C14)] = "less_freq"


# Translate C15 -----------------------------------------------------------
length(unique(Train$C15)) # 8
x = Train_matrix[,'C15']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]

Labs = names(out)

TransTable_C15 = c(rep("more_freq", 2), rep("less_freq", 4), rep("least_freq", 2))
names(TransTable_C15) = Labs

# translation table
Tables[[9]] = TransTable_C15

# apply the table to original column and deal with NAs
Train$C15 = TransTable_C15[x]
#Train$C15[is.na(Train$C15)] = "less_freq"


# Translate C16 -----------------------------------------------------------
length(unique(Train$C16)) # 9 

x = Train_matrix[,'C16']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]

Labs = names(out)

#       1024        250        768        320        480         50 
# 0.42551566 0.42302096 0.27886057 0.26253521 0.21102797 0.15835357 
#         36         90         20 
# 0.12472417 0.05932161 0.01852538 

TransTable_C16 = c(rep("more_freq", 2), rep("less_freq", 5), rep("least_freq", 2))
names(TransTable_C16) = Labs

# translation table
Tables[[10]] = TransTable_C16

# apply the table to original column and deal with NAs
Train$C16 = TransTable_C16[x]
#Train$C16[is.na(Train$C16)] = "less_freq"


# Translate C17 -----------------------------------------------------------
length(unique(Train$C17)) # 407

x = Train_matrix[,'C17']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]
Labs = names(out)

# out is distributed from 0.8 to 0, so we will use the criteria as follows:
sum(out >= 0.50) # 9
sum(out > 0.25 & out < 0.50) # 47
sum(out <= 0.25) # 351

TransTable_C17 = c(rep("more_freq", 9), rep("less_freq", 47), rep("least_freq", 351))
names(TransTable_C17) = Labs

# translation table
Tables[[11]] = TransTable_C17

# apply the table to original column and deal with NAs
Train$C17 = TransTable_C17[x]
#Train$C17[is.na(Train$C17)] = "less_freq"


# Translate C18 -----------------------------------------------------------
length(unique(Train$C18)) #4

x = Train_matrix[,'C18']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]
Labs = names(out)

#          2          0          3          1 
# 0.29508653 0.15909508 0.14822801 0.03369873 

TransTable_C18 = c(rep("more_freq", 1), rep("less_freq", 2), rep("least_freq", 1))
names(TransTable_C18) = Labs

# translation table
Tables[[12]] = TransTable_C18

# apply the table to original column and deal with NAs
Train$C18 = TransTable_C18[x]
#Train$C18[is.na(Train$C18)] = "less_freq"


# Translate C19 -----------------------------------------------------------
length(unique(Train$C19)) #66

x = Train_matrix[,'C19']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]
Labs = names(out)

sum(out >= 0.20) # 12
sum(out > 0.10 & out < 0.20) # 28
sum(out <= 0.10) # 26

TransTable_C19 = c(rep("more_freq", 12), rep("less_freq", 28), rep("least_freq", 26))
names(TransTable_C19) = Labs

# translation table
Tables[[13]] = TransTable_C19

# apply the table to original column and deal with NAs
Train$C19 = TransTable_C19[x]
#Train$C19[is.na(Train$C19)] = "less_freq"


# Translate C20 -----------------------------------------------------------
length(unique(Train$C20)) # 171

x = Train_matrix[,'C20']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]
Labs = names(out)

sum(out >= 0.30) # 3
sum(out > 0.00 & out < 0.30) # 162
sum(out <= 0.00) # 6

TransTable_C20 = c(rep("more_freq", 3), rep("less_freq", 162), rep("least_freq", 6))
names(TransTable_C20) = Labs

# translation table
Tables[[14]] = TransTable_C20

# apply the table to original column and deal with NAs
Train$C20 = TransTable_C20[x]
#Train$C20[is.na(Train$C20)] = "less_freq"

# Translate C21 -----------------------------------------------------------
length(unique(Train$C21)) # 55

x = Train_matrix[,'C21']
out = tapply(y,x, FUN=mean)
out = out[order(out, decreasing = T)]
Labs = names(out)

TransTable_C21 = c(rep("more_freq", 6), rep("less_freq", 29), rep("least_freq", 20))
names(TransTable_C21) = Labs

# translation table
Tables[[15]] = TransTable_C21

# apply the table to original column and deal with NAs
Train$C21 = TransTable_C21[x]
#Train$C21[is.na(Train$C21)] = "less_freq"

# additional function to check frequency ---------------------------------------------
Train %>% 
  group_by(site_domain) %>%
  summarize(counts = n()) %>%
  summarize(percentage = counts/sum(counts)) %>%
  arrange(desc(percentage)) %>%
  mutate(running_percentage = cumsum(percentage)) %>%
  head(20)

# change categorical variables to dummy variables ------------------------------------
Train = as.data.frame(Train)
Train_dummy = dummy_cols(Train, select_columns = c("C1","banner_pos","site_category","app_category" ,"device_model","device_type","device_conn_type", "C14", "C15","C16","C17", "C18","C19","C20","C21"), remove_first_dummy = TRUE)
Train_dummy = Train_dummy[-c(2:16)]
 
# take sample from Train ----------------------------------------------------------
rows <- sample(nrow(Train_dummy))
randomTrain <- Train_dummy[rows,]
fwrite(randomTrain, "ProcessedTraining.csv")

randomTrain <- fread("ProcessedTraining.csv", header = TRUE)

# now randomly shuffle the training and val datasets
TrainingDataset <- randomTrain[1:3000000,]
fwrite(TrainingDataset, "newTrainingDataset.csv")

ValidationDataset <- randomTrain[3000001:4000000,]
fwrite(ValidationDataset, "newValidationDataset.csv")

# transform the test data ----------------------------------------------------------
Test <- fread("ProjectTestData.csv")

# take a look at total column names
colnames(Test)

# remove id, device_id, device_ipTest[, id := NULL] 

Test[, device_id := NULL] 
Test[, device_ip := NULL] 
Test[, site_id := NULL]
Test[, site_domain := NULL]
Test[, app_id := NULL] 
Test[, app_domain := NULL] 

# break up hour into time category for hour of day and date for day
Test[, time := hour %% 100]
Test[, Morning := 0]; Test[Test$time >= 5 & Test$time < 11, Morning := 1]
Test[, Day := 0]; Test[Test$time >= 11 & Test$time < 17, Day := 1]
Test[, Evening := 0]; Test[Test$time >= 17 & Test$time < 20, Evening := 1]
Test[, Night := 0]; Test[Test$time >= 20 & Test$time < 24, Night := 1];Test[Test$time == 0, Night := 1]

Test[, time := NULL]

Test[, date := (hour %% 10000 - hour %% 100)/100]
Test[, Tuesday := 0]; Test[Test$date == 21|Test$date == 28, Tuesday := 1]
Test[, Monday := 0]; Test[Test$date == 27, Monday := 1]
Test[, Wednesday := 0]; Test[Test$date == 22, Wednesday := 1]
Test[, Thursday := 0]; Test[Test$date == 23, Thursday := 1]
Test[, Saturday := 0]; Test[Test$date == 25, Saturday := 1]
Test[, Sunday := 0]; Test[Test$date == 26, Sunday := 1]

Test[, date := NULL]
Test[, hour := NULL]

Test_matrix = as.matrix(Test)

for (i in 2:16){
  x = Test_matrix[,i]
  Test[,i] = Tables[[i-1]][x]
  #Test[,i][is.na(Test[,i])] = "less_freq"
}

Test[,2][is.na(Test[,2])] = "less_freq"

Test[,3][is.na(Test[,3])] = "less_freq"

Test[,4][is.na(Test[,4])] = "less_freq"

Test[,5][is.na(Test[,5])] = "less_freq"

Test[,6][is.na(Test[,6])] = "less_freq"

Test[,7][is.na(Test[,7])] = "less_freq"

Test[,8][is.na(Test[,8])] = "less_freq"

Test[,9][is.na(Test[,9])] = "less_freq"

Test[,10][is.na(Test[,10])] = "less_freq"

Test[,11][is.na(Test[,11])] = "less_freq"

Test[,12][is.na(Test[,12])] = "less_freq"

Test[,13][is.na(Test[,13])] = "less_freq"

Test[,14][is.na(Test[,14])] = "less_freq"

Test[,15][is.na(Test[,15])] = "less_freq"

Test[,16][is.na(Test[,16])] = "less_freq"

# change categorical variables to dummy variables ------------------------------------
Test = as.data.frame(Test)
Test_dummy = dummy_cols(Test, select_columns = c("C1","banner_pos","site_category","app_category" ,"device_model","device_type","device_conn_type", "C14", "C15","C16","C17", "C18","C19","C20","C21"), remove_first_dummy = TRUE)

# site id, site domain, app id, app domain, device model 
Test_dummy = Test_dummy[-c(2:16)]

fwrite(Test_dummy, "ProcessedTest.csv")

######################## Retrieve Predictions ########################

# We need to read in XGBoost predictions since those were done in Python
predXGBTest <- fread("TestYHatFrommnXGB.csv", header = TRUE)
colnames(predXGBTest) <- "predXGBTest"

# put model predictions together into a data frame and we will use ensemble method to take the average of the 2 best models
TestDataPreds <- data.frame(predXGBTest)

# take the column value from XGB models and add it to column P(click)
TestDataPreds$`P(click)` <- rowMeans(subset(TestDataPreds, select = c( predXGBTest)))

# read in the submission file with correct data types
Data <- read.table("ProjectSubmission-TeamX.csv",colClasses=c("character","numeric"),header=T,sep=",")

# put the probabilities in Data[[2]]
Data[[2]] <- TestDataPreds$`P(click)`

# round to 10 digits accuracy and prevent scientific notation
# this converts Data[[2]] to strings
Data[[2]] <- format(round(Data[[2]],10), scientific = FALSE)

# write out the data in the correct format
write.table(Data,file="ProjectSubmission-Team6.csv",quote=F,sep=",",
            row.names=F,col.names=c("id","P(click)"))
