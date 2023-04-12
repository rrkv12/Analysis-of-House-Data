#####################################Q1 Part1#######################################################################

#install.packages('tidyr')
#install.packages("mice")
#install.packages("naniar")
#install.packages("ggplot2")
library(ggplot2)
library(naniar)
library(tidyr)
library(dplyr)
library(purrr)
library(mice)
library(caret)
options(scipen=999)  #display decimal values, rather than scientific notation

"The first processing step is to convert the categorical variables from the type - 'character'- to the type - 'factor'. This is done by setting stringsAsFactors = TRUE when the data is loaded"

data<-read.csv("house-data.csv",header=T, stringsAsFactors=TRUE)
str(data)

# Identify variables with NEAR ZERO VARIANCE. This problem arises because a very high proportion of observations in a column have the same value.
# This is important because Mean square error is inversely proportional to variance. So near zero variances give larege errors.

#dim(data)

data[data==""] <- NA
nzv_vals <- nearZeroVar(data, saveMetrics = TRUE)
#dim(nzv_vals)

nzv_sorted <- arrange(nzv_vals, desc(freqRatio))
head(nzv_sorted)

#nzv = near zero variance.

#DROP 14 COLUMNS
#The output indicates that 6 variables have near zero variances. We will drop these columns. In addition we drop columns where the number of #observations in any level exceed 90% of the total observations in the column. This also applies to columns where NA meets that criterion. On this #basis, there are 8 additional columns to be dropped and these are: "MiscFeatures", "PoolQC", "PavedDrive", "GarageCondition", "Functional", "Heating", #"BsmtCond" and"Alley". 14 columns are dropped in total. We doublecheck column names and their index.

data_drop<-data %>% select(-6,-42,-29,-4,-10,-17,-45,-43,-41,-40,-36,-26,-24,-5)


#REPLACE NA IN "Fence" WITH "None" because NA here means 'no fence'

data_drop<-data_drop %>% mutate(Fence = ifelse(is.na(Fence), "None", Fence))
table(data_drop$Fence)

#Define the factors which are ordinal, ie, those that have a natural ranking and change Python's alphabetic ranking to specifying rank from lowest to #highest

data_drop$OverallQual<-factor(data_drop$OverallQual,levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),ordered = TRUE, exclude = NULL) 
#results<-table(data_drop$OverallQual)
#results

data_drop$OverallCond<-factor(data_drop$OverallCond, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),ordered = TRUE, exclude = NULL) 
#results<-table(data_drop$OverallCond)
#results

data_drop$ExterQual<-factor(data_drop$ExterQual,levels=c("Fa", "TA", "Gd", "Ex"),ordered = TRUE, exclude = NULL)  
#results<-table(data_drop$ExterQual)
#results

data_drop$ExterCond<-factor(data_drop$ExterCond, levels=c("Po","TA", "Gd", "Ex"),ordered = TRUE, exclude = NULL)         
#results<-table(data_drop$ExterCond)
#results

data_drop$BsmtQual<-factor(data_drop$BsmtQual,levels=c("Fa", "TA", "Gd", "Ex"),ordered = TRUE, exclude = NULL) 
#results<-table(data_drop$BsmtQual)
#results

data_drop$KitchenQual<-factor(data_drop$KitchenQual,levels=c("Fa", "TA", "Gd", "Ex"),ordered = TRUE, exclude = NULL)           
#results<-table(data_drop$KitchenQual)
#results

data_drop$Fence<-factor(data_drop$Fence,levels=c("None","4", "2", "3", "1"), ordered = TRUE, exclude = NULL)      
results<-table(data_drop$Fence)
results

#MAKE A COPY OF DATA_DROP
data<-data_drop

#CREATE NEW 'AGE' COLUMN BY SUBTRACTING YEARBUILT FROM YEAR OF SALE OF HOUSE

data_age<-data %>% mutate(Age <- YrSold-YearBuilt)
data_new<-rename(data_age, Age = "Age <- YrSold - YearBuilt")

#CREATE NEW MARKET CRISIS COLUMN "MktCrisis"

data<-data_new %>% mutate(Calc <- (MoSold/12+YrSold)-2009.1)
data_newcol<-rename(data, MktCrisis = "Calc <- (MoSold/12 + YrSold) - 2009.1")

#DROP THE 3 DATE COLUMNS IN THE ORIGINAL DATASET BECAUSE OF THEIR CONVERSION TO 2 NEW COLUMNS

data_newcol$YearBuilt <- data_newcol$MoSold <- data_newcol$YrSold <- NULL
data_clean <- data_newcol
str(data_clean)

#SAVE ALL THE CHANGES TO THE ORIGINAL DATASET TO THE NEW DATASET "data_clean"
#save(data_clean, file = "C:/Users/Inthiran Moodley/OneDrive/Documents/Essex University/Academic/2021/MA321_/Assignments/Assignment2/data_clean.Rdata")

###############################Q1 Part2#####################################################################
#Counting the missing values in the each column.
na_count1 <- sapply(data_clean, function(x) sum(length(which(is.na(x)))))
na_count1
#Visualization
#install.packages("tidyr")
#install.packages("dplyr")
#library(tidyr)
#library(dplyr)
#library(purrr)
data_clean[-1] %>% keep(is.numeric) %>%   #plot of histograms for all variables
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(color= "blue", fill = "blue")
#Relationship of missing values with in the columns
#install.packages("naniar")
#install.packages("ggplot2")
#library(ggplot2)
#library(naniar)
gg_miss_upset(data_clean) #Relationship of missing values within the columns
vis_miss(data_clean)
gg_miss_case(data_clean) 
gg_miss_var(data_clean, show_pct = TRUE)
gg_miss_var(data_clean)
gg_miss_fct(x = data_clean, fct = SalePrice)  #Shows the number of missing in each col
gg_miss_case_cumsum(data_clean) #Cumulative of missing values
gg_miss_which(data_clean) #Column without Missing variable is the black one
ggplot(data_clean) + aes(x = SalePrice) + geom_histogram(binwidth = 10, color= "blue", fill = "blue" )
#Scatter Plots of the various predictor variables with the response varable SalePrice.
#Using log value of SalePrice instead of direct SalePrice since its a big value.
ggplot(data_clean, aes(OverallQual,log(SalePrice))) + geom_point() +xlab("Overall Quality ") + ylab("SalesPrice")
ggplot(data_clean, aes(OverallCond,log(SalePrice))) + geom_point() +xlab("Overall Condition ") +
  ylab("SalesPrice")
ggplot(data_clean, aes(LotFrontage,log(SalePrice))) + geom_point() +xlab("Frontage ") +
  ylab("SalesPrice")
ggplot(data_clean, aes(LotArea,log(SalePrice))) + geom_point() +xlab("Area") +
  ylab("SalesPrice")
##ggplot(data_clean, aes(LotArea,log(SalePrice))) + geom_miss_point()
ggplot(data_clean, aes(MasVnrArea,log(SalePrice))) + geom_point() +xlab("MasVnrArea") +
  ylab("SalesPrice")
ggplot(data_clean, aes(TotalBsmtSF,log(SalePrice))) + geom_point() +xlab("TotalBsmtSF") +
  ylab("SalesPrice")
ggplot(data_clean, aes(X1stFlrSF,log(SalePrice))) + geom_point() +xlab("X1stFlrSF") +
  ylab("SalesPrice")
ggplot(data_clean, aes(X2ndFlrSF,log(SalePrice))) + geom_point() +xlab("X2ndFlrSF") +
  ylab("SalesPrice")
ggplot(data_clean, aes(GrLivArea,log(SalePrice))) + geom_point() +xlab("GrLivArea") +
  ylab("SalesPrice")
ggplot(data_clean, aes(FullBath,log(SalePrice))) + geom_point() +xlab("FullBath") +
  ylab("SalesPrice")
ggplot(data_clean, aes(BedroomAbvGr,log(SalePrice))) + geom_point() +xlab("BedroomAbvGr") +
  ylab("SalesPrice")
ggplot(data_clean, aes(KitchenAbvGr,log(SalePrice))) + geom_point() +xlab("KitchenAbvGr") +
  ylab("SalesPrice")
ggplot(data_clean, aes(TotRmsAbvGrd,log(SalePrice))) + geom_point() +xlab("TotalRmsAbvGrd") +
  ylab("SalesPrice")
ggplot(data_clean, aes(Fireplaces,log(SalePrice))) + geom_point() +xlab("Fireplaces") +
  ylab("SalesPrice")
ggplot(data_clean, aes(GarageArea,log(SalePrice))) + geom_point() +xlab("GarageArea") +
  ylab("SalesPrice")
ggplot(data_clean, aes(MiscVal,log(SalePrice))) + geom_point() +xlab("MiscVal") +
  ylab("SalesPrice")
ggplot(data_clean, aes(Age,log(SalePrice))) + geom_point() +xlab("Age") +
  ylab("SalesPrice")
ggplot(data_clean, aes(MktCrisis,log(SalePrice))) + geom_point() +xlab("MktCrisis") +
  ylab("SalesPrice")
#Treating missing Values
#install.packages("mice")
#library(mice)
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(data_clean, 2, p)
md.pairs(data_clean)
md.pattern(data_clean, plot = T)

imputations <- mice(data_clean, seed = 23884, method = "cart", m=10)
head(imputations)
n_miss(imputations) #Checking whether the number of missing values is zero after imputation.
#Impute2 <- complete(imputations, 2)
#print(Impute2)
#Impute10 <- complete(imputations, 10)
#print(Impute10)
new_data_clean <- complete(imputations)
View(new_data_clean)
head(new_data_clean, 10)
imputations$imp
stripplot(imputations, pch = 20, cex = 1.2)
xyplot(imputations, MasVnrArea ~ LotFrontage | .imp, pch = 20, cex = 1.4)

#Save the new_data_clean data-set.
#save(new_data_clean, file = "C:/Essex uni - assignments/321 - Assignment/RV_321_final works/new_data_clean.Rdata"
#Estimate a linear regression model for the response "Sale Price" with the predictor variables.
#Estimate a linear model with sales price as response variable and other variables as predictor variables.
lm1<-lm(log(SalePrice)~LotFrontage+LotArea+OverallQual+OverallCond+MasVnrArea+TotalBsmtSF+X1stFlrSF+X2ndFlrSF+GrLivArea+FullBath+BedroomAbvGr+KitchenAbvGr+Fireplaces+GarageArea+MiscVal+Age+MktCrisis,data=new_data_clean) 
summary(lm1)
#Find the standard residuals of the linear model lm1
stresidual_lm1<-rstandard(lm1) 
#Plot the standard residuals against the fitted values of the linear model lm1.
plot(lm1$fitted.values,stresidual_lm1,pch=16,ylab="Standard Residuals",xlab="Fitted Values",ylim=c(-20,20),main="Standard Residuals Vs Fitted Values")
abline(h=0)
abline(h=5,lty=2)
abline(h=-5,lty=2)
#QQ-plot for the linear model (lm1)
qqnorm(stresidual_lm1, ylab="Standardized Residuals",xlab="Normal Scores", main="QQ Plot for lm1" )
qqline(stresidual_lm1)
plot(lm1) #Alternative way to get the above plots.
######################################################################################################
###############################Q2#####################################################################
install.packages("dplyr")
install.packages("ggplot2")
install.packages("purrr")
install.packages("caTools")
install.packages("nnet")
install.packages("mice")
install.packages("caret")
install.packages("Mass")
install.packages("randomForest")
library(dplyr)
library(ggplot2)
library(purrr)
library(caTools)
library(nnet)
library(mice)
library(caret)
library(MASS)
library(randomForest)

set.seed(123) #we are setting seed to lock the results
house_data=read.csv("house-data.csv",stringsAsFactors = T) #loading the dataset
View(house_data)
dim(house_data)

#checking the missing values in every column 

map(house_data, ~sum(is.na(.)))
str(house_data)

#putting the levels for the conditions from the question

house_data$OverallCond <- ifelse(house_data$OverallCond>=1 & house_data$OverallCond <= 3, "Poor", "Good")
house_data$OverallCond <- ifelse(house_data$OverallCond >= 4 & house_data$OverallCond <= 6, "Average", house_data$OverallCond)
house_data$OverallCond <- ifelse(house_data$OverallCond >= 7 & house_data$OverallCond <= 10, "Good", house_data$OverallCond)

#Checking the Overall house condition values
table(house_data$OverallCond)
house_data$OverallCond=as.factor(house_data$OverallCond)

#dropping columns
house_data_2 <- subset(house_data_2, select = -c(Alley, Fence,PoolQC,MiscFeature))
View(house_data_2)

#imputing the columns with missing values
imputation= mice(house_data_2[,c(2,18,22,23,37,39)])
print(imputation)

#imputing the data
new_data=complete(imputation,1)
new_data

#replacing the imputed columns
house_data_2[,c(2,18,22,23,37,39)]=new_data[,c(1,2,3,4,5,6)]
sum(is.na(new_data)) # checking the number of missing values
summary(new_data) #printing the summary of the cleaned data

################# Question 2a ###################

#the dataset is being split into train and test models using split function

split= sample.split(house_data_2$Overallcond, SplitRatio = 0.8)

#train model
train=subset(house_data_2,split==TRUE)
#test model
test=subset(house_data_2,split==FALSE)

#multinomial logistic regression is fitted into the training data

train$Overallcond <- relevel(train$Overallcond, ref = "Average")
multinom.fit <- multinom(Overallcond~., data = train)

#printing summary
summary(multinom.fit)
head(probability.table <- fitted(multinom.fit))

#trained data prediction 1
prediction <- predict(multinom.fit, newdata = train, "class")
#the train model's first confusion matrix
con_mat1 = confusionMatrix(prediction, train$Overallcond)
con_mat1


#the train model's second confusion matrix
prediction_2= predict(multinom.fit, newdata = test, "class")
#the test model's second confusion matrix
con_mat2=confusionMatrix(prediction_2, test$Overallcond)
con_mat2


#######################Question 2b ###########################

######### Random forest model #########

# #the dataset is being split into train and test models using split function

split_2 = sample.split(house_data_2$OverallCond, SplitRatio = 0.8)
train_2 = subset(house_data_2,split_2 == TRUE)
test_2 = subset(house_data_2,split_2 == FALSE)

#Random forest on training data set


# The default n tree value is 500, so we would be having the levels as compared to that with 
#respect to the data

RanForest <- randomForest(OverallCond~., data=train_2)

print(RanForest)
attributes(RanForest)

#the train model's first confusion matrix
prediction_2b1 <- predict(RanForest, newdata = train_2)
##the test model's first confusion matrix
con_mat2b1 = confusionMatrix(prediction_2b1, train_2$OverallCond)


##the train model's second prediction
prediction_2b2 <- predict(RanForest, newdata = test_2)
##the test model's second confusion matrix
con_mat2b2 = confusionMatrix(prediction_2b2, test_2$OverallCond)

################################Q3#######################################################################

knitr::opts_chunk$set(echo = TRUE)
library(psych)
library(ggplot2)
data <- read.csv("house-data.csv")
summary(data)
describe(data)
datesold <- data$MoSold + data$YrSold
ggplot(data, aes(as.factor(OverallCond), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#FF6666") + 
  labs(y = "Sale Price", x = "Overall Condition")
ggplot(data, aes(as.factor(GarageType), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#424C98") + 
  labs(y = "SalePrice", x = "GarageType")
ggplot(data, aes(as.factor(SaleCondition), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#416415") + 
  labs(y = "SalePrice", x = "Sale Condition")
ggplot(data, aes(as.factor(SaleType), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#E67A35") + 
  labs(y = "SalePrice", x = "Sale Type")
ggplot(data, aes(as.factor(datesold), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#BCC022") + 
  labs(y = "SalePrice", x = "Date Sold")
ggplot(data, aes(as.factor(Functional), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#540B62") + 
  labs(y = "SalePrice", x = "Functionality")
ggplot(data, aes(as.factor(BedroomAbvGr), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#4E759A") + 
  labs(y = "SalePrice", x = "No. of Bedrooms")
ggplot(data, aes(as.factor(Heating), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#45523B") + 
  labs(y = "SalePrice", x = "Heating Type")
ggplot(data, aes(as.factor(Foundation), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#AA7784") + 
  labs(y = "SalePrice", x = "Foundation")
ggplot(data, aes(YearBuilt, SalePrice)) + 
  geom_line(color = "#AA9677") + 
  labs(y = "SalePrice", x = "Year Built") + 
  geom_point()
ggplot(data, aes(as.factor(HouseStyle), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#7EC288") + 
  labs(y = "SalePrice", x = "House Style")
ggplot(data, aes(as.factor(Condition1), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#7E81C2") + 
  labs(y = "SalePrice", x = "Proximity to Conditions")
ggplot(data, aes(as.factor(Neighborhood), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#4A3F32") + 
  labs(y = "SalePrice", x = "Neighborhood")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(data, aes(as.factor(Utilities), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#1A0F7F") + 
  labs(y = "SalePrice", x = "Utilities")
ggplot(data, aes(as.factor(Alley), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#500A18") + 
  labs(y = "SalePrice", x = "Alley Access")
ggplot(data, aes(as.factor(LotConfig), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#7F770F") + 
  labs(y = "SalePrice", x = "Lot Configuration")
ggplot(data, aes(as.factor(ExterQual), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#A0AA77") + 
  labs(y = "SalePrice", x = "Exterior Quality")
ggplot(data, aes(as.factor(KitchenQual), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#853A52") + 
  labs(y = "SalePrice", x = "Kitchen Quality")
ggplot(data, aes(as.factor(Fireplaces), SalePrice)) + 
  geom_bar(stat = "identity",fill = "#3A8582") + 
  labs(y = "SalePrice", x = "Fireplaces")
library(dplyr)
library(caret)
library(nnet)
library(rpart)
library(e1071)

f <- function (x) {
  if ((x>=1)&(x<=3)) {
    x <- "Poor"}
  else if ((x>=4)&(x<=6)) {
    x<-"Average" }
  else
  {
    x<-"Good" }
}

data$OverallCond <- sapply(data$OverallCond,f)
data <- data %>% mutate(GarageType = ifelse(GarageArea == 0, "No_Garage", GarageType))
data <- data %>% mutate(GarageCond = ifelse(GarageArea == 0, "No_Garage", GarageCond))
data <- data %>% mutate(BsmtCond = ifelse(TotalBsmtSF == 0, "No_Basement", BsmtCond))
data <- data %>% mutate(BsmtQual = ifelse(TotalBsmtSF == 0, "No_Basement", BsmtQual))
data <- data %>% mutate(MasVnrArea = ifelse(is.na(MasVnrArea), 0, MasVnrArea))
data <- subset(data, select = -c(PoolQC,Fence,MiscFeature,Alley,LotFrontage,RoofMatl,Exterior1st, Id))
data$OverallCond <- as.factor(data$OverallCond)
set.seed(430)
default_idx = createDataPartition(data$OverallCond, p = 0.75, list = FALSE)
train_data = data[default_idx, ]
test_data = data[-default_idx, ]

preProcValues <- preProcess(train_data, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, train_data)
testTransformed <- predict(preProcValues, test_data)
mod <- multinom(OverallCond ~ ., trainTransformed)
summary(mod)
pred<-predict(mod, testTransformed, type='class')

xtab <- table(pred, test_data$OverallCond)
confusionMatrix(xtab)
library(kernlab)
trainTransformed[] <- data.matrix(trainTransformed)
testTransformed[] <- data.matrix(testTransformed)
str(testTransformed)
train_data$OverallCond <- as.factor(train_data$OverallCond)
dat <- data.frame( train_data , train_data$OverallCond)
test_data$OverallCond <- as.factor(test_data$OverallCond)
dat1 <- data.frame( test_data , test_data$OverallCond)
svm_Linear <- svm(OverallCond~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
svm_Linear
svm_predict <-predict(svm_Linear, dat, type = 'class')
svm_predict
xtab <- table(svm_predict, dat$OverallCond)
confusionMatrix(xtab)
evaluation_res <- read.csv("house-data.csv")
print(evaluation_res)
#################################################################################################

