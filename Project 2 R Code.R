###################################
#### Basic Info from Project 1 ####
###################################

##To import my dataset directly from the internet
data<- read.csv('https://raw.githubusercontent.com/shiranli/ECO494-Project-I/main/raw%20data.csv')

##Checks the dimensions
dim(data)

##Shows the first 6 observations
head(data)

##Shows the last six observations.
tail(data)

##See the data in a summarized spreadsheet format in a new tab.
View(data)

##Generate the "six number summary" statistics
summary(data) 

##data cleaning process
##rename variables 
names(data)[1:8] <- c("countries", "inflation", "GDP", "trade", "income", "gini"
                      , "populationgrowth", "developedcountry")
str(data)

##create new dataset from starting point
##convert % to decimals
cleandata<-data
cleandata[,2] <- as.numeric(sub("%","",cleandata[,2]))/100
cleandata[,3] <- as.numeric(sub("%","",cleandata[,3]))/100
cleandata[,4] <- as.numeric(sub("%","",cleandata[,4]))/100
cleandata[,7] <- as.numeric(sub("%","",cleandata[,7]))/100
View(cleandata)



#########################
#### Regression Task ####
#########################

##LOAD THE GGPLOT2, PLYR, and TSERIES LIBRARIES
library(ggplot2) #for ggplot system and preloaded datasets
library(plyr) #for ddply()
library("tseries")


#INCORPORATING NONLINEAR AND LOGARITHMIC TRANSFORMATIONS OF INCOME
cleandata$income2<-cleandata$incomen^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
cleandata$ln_income<-log(cleandata$income) #LOGARITHMIC TRANSFORMATION 

#fraction of sample to be used for training
p<-.7

#number of observations (rows) in the dataframe
obs_count<-dim(cleandata)[1]

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size
#set the seed to make your partition reproducible
set.seed(1234)
#create a vector with the shuffled row numbers of the original dataset
train_ind <- sample(obs_count, size = training_size)

Training <- cleandata[train_ind, ] #pulls random rows for training
Testing <- cleandata[-train_ind, ] #pulls random rows for testing

#CHECKING THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)


#BUILDING THE MODEL FROM THE TRAINING DATA
M1 <- lm(inflation ~ income, Training)
summary(M1) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values
M1$coefficients #RETURNS BETA ESTIMATES
hist(M1$residuals) #PLOT THEM!
jarque.bera.test(M1$residuals) #TEST FOR NORMLAITY!

#Plot the Relationship Graph
ggplot(cleandata, aes(x = inflation, y = income)) + 
  geom_point() +
  geom_smooth(method ='lm')

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$inflation)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$inflation)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions_1 <- predict(M1, list(income=x_grid))
plot(Training$inflation ~ Training$income, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$inflation ~ Testing$income, col='red', pch=3)

#BUILDING THE SECOND MODEL FROM THE TRAINING DATA
M2 <- lm(inflation ~ income + GDP, Training)
summary(M2) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(M2$fitted.values) #these are the same as the fitted values
M2$coefficients #RETURNS BETA ESTIMATES
hist(M2$residuals) #PLOT THEM!
jarque.bera.test(M2$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$inflation)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$inflation)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#BUILDING THE THIRD MODEL FROM THE TRAINING DATA
M3 <- lm(inflation ~ income + GDP + trade, Training)
summary(M3) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values) #these are the same as the fitted values
M3$coefficients #RETURNS BETA ESTIMATES
hist(M3$residuals) #PLOT THEM!
jarque.bera.test(M3$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$inflation)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$inflation)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#BUILDING THE FOURTH MODEL FROM THE TRAINING DATA
M4 <- lm(inflation ~ income + GDP + populationgrowth, Training)
summary(M4) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(M4$fitted.values) #these are the same as the fitted values
M4$coefficients #RETURNS BETA ESTIMATES
hist(M4$residuals) #PLOT THEM!
jarque.bera.test(M4$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$inflation)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$inflation)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#BUILDING THE QUADRATIC MODEL FROM THE TRAINING DATA
M5 <- lm(inflation ~ income + income2, Training)
summary(M5) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_5_IN <- predict(M5, Training) #generate predictions on the (in-sample) training data
View(PRED_5_IN)
View(M5$fitted.values) #these are the same as the fitted values
hist(M5$residuals) #PLOT THEM!
jarque.bera.test(M5$residuals) #TEST FOR NORMLAITY!

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_5_OUT <- predict(M5, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_5_IN<-sqrt(sum((PRED_5_IN-Training$inlflation)^2)/length(PRED_5_IN))  #computes in-sample error
RMSE_5_OUT<-sqrt(sum((PRED_5_OUT-Testing$inflation)^2)/length(PRED_5_OUT)) #computes out-of-sample 

RMSE_5_IN #IN-SAMPLE ERROR
RMSE_5_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions_5 <- predict(M5, list(income=x_grid, income2=x_grid^2))
plot(Training$inflation ~ Training$income, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$inflation ~ Testing$income, col='red', pch=3)

#BUILDING THE LOGARITHMIC MODEL FROM THE TRAINING DATA
M6 <- lm(inflation ~ ln_income, Training)
summary(M6) #generates summary diagnostic output

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_6_IN <- predict(M6, Training) #generate predictions on the (in-sample) training data
View(PRED_6_IN)
View(M6$fitted.values) #these are the same as the fitted values
hist(M6$residuals) #PLOT THEM!
jarque.bera.test(M6$residuals) #TEST FOR NORMLAITY!

#Plot the Relationship Graph
ggplot(cleandata, aes(x = inflation, y = ln_income)) + 
  geom_point() +
  geom_smooth(method ='lm')

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_6_OUT <- predict(M6, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_6_IN<-sqrt(sum((PRED_6_IN-Training$inflation)^2)/length(PRED_6_IN))  #computes in-sample error
RMSE_6_OUT<-sqrt(sum((PRED_6_OUT-Testing$inflation)^2)/length(PRED_6_OUT)) #computes out-of-sample 

RMSE_6_IN #IN-SAMPLE ERROR
RMSE_6_OUT #OUT-OF-SAMPLE ERROR

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
predictions_6 <- predict(M6, list(ln_income=log(x_grid)))
plot(Training$inflation ~ Training$income, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$inflation ~ Testing$income, col='red', pch=3)


########################################
########### MODEL COMPARISON ###########
########################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #FIRST MODEL WITH LINEAR TERM
RMSE_2_IN #SECOND MODEL WITH LINEAR TERM
RMSE_3_IN #THIRD MODEL WITH LINEAR TERM
RMSE_4_IN #FOURTH MODEL WITH LINEAR TERM
RMSE_5_IN #MODEL WITH LINEAR, AND QUADRATIC TERM
RMSE_6_IN #LOGARITHMIC MODEL

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #FIRST MODEL WITH LINEAR TERM
RMSE_2_OUT #SECOND MODEL WITH LINEAR TERM
RMSE_3_OUT #THIRD MODEL WITH LINEAR TERM
RMSE_4_OUT #FOURTH MODEL WITH LINEAR TERM
RMSE_5_OUT #MODEL WITH LINEAR, AND QUADRATIC TERM
RMSE_6_OUT #LOGARITHMIC MODEL

########################################################
###PLOTTING THE REGRESSION MODELS AGAINST ONE ANOTHER###
########################################################
x_grid <- seq(0,8,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$inflation ~ Training$income, col='blue')
predictions_1 <- predict(M1, list(income=x_grid))
predictions_5 <- predict(M2, list(income=x_grid, income2=x_grid^2))
predictions_6 <- predict(M4, list(ln_income=log(x_grid)))
lines(x_grid, predictions_1, col='darkgreen', lwd=3) #PLOTS M1
lines(x_grid, predictions_5, col='green', lwd=3) #PLOTS M5
lines(x_grid, predictions_6, col='orange', lwd=3) #PLOTS M6
points(Testing$inflation ~ Testing$income, col='red', pch=3)











