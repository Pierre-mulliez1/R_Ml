#Author: Pierre Mulliez,
#Last edited: 17/06/2021
#Project: Covid prediction
#Machine learning

#Predict cases and fatilities for the right set of dates 


library(data.table)  
library(dplyr)
#install.packages('caret', dependencies = TRUE)
library(caret)
#install.packages("randomForest")
library(randomForest)
library(foreach)
library(doParallel)
library(Metrics)
#install.packages("glmnet")
library(glmnet)


getwd()
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

path_train <- "data/train.csv"
path_test <- "data/test.csv"
path_location <- "data/country_location.xlsx"
datrain <- read.csv(path_train)
datest <- read.csv(path_test)

#Continent and sub region dataset imported (instead of hard code) this will allow us to use features such as dummifying 
#or categorical values in random forest
continent <- read.csv('data/countryContinent.csv')
continent <- subset(continent, select = c(country, sub_region))
continent <- na.replace(continent, 'unknown')

#merge th econtinent dataset
merging <- function(data){
  #reintialise data frame
  dat <- data.frame()
  dat <- merge(data, continent, by.x = 'Country_Region', by.y= 'country',all.x = TRUE )
  print(head(dat))
  print('Summary of dataset: ')
  print(summary(dat))
    return(dat)
}


#default fatalities as a target
####### choose the target here (cases or fatalities)
target <- function(data, targ = "Fatalities"){
  dat <- merging(data)
  if (targ == "Fatalities"){
    dat$target <- dat$Fatalities 
  }
  else{
    dat$target <- dat$ConfirmedCases 
  }
  
  if ("Fatalities" %in% colnames(data)){
  #dropping states as many nulls and region as cannot use it in model for now
  dat <- subset(dat,select =-c(Fatalities, Id,Province_State,ConfirmedCases))
  }
  else  {
    print(colnames(data))
    dat <- subset(dat,select =-c(Province_State))
  }
    
  #Deal with na generated ?
  a <- length(dat$target)
  dato <- na.omit(dat)
  b <- length(dato$target)
  
  omitted <- a - b
  print(paste("omitted rows = ", omitted))
  #condition to be set ONLY if test set has some nulls in na omit
  if (omitted > 1){
    return(dato)
  }
  else {
    return(dato)
  }
    
}

######DUMIFY
#use only for unsplitted model for analysis in stacking (otherwise same dumy for the whole collumn)
dum <- function(dat){
  oh_model <- dummyVars(dat[3], data = dat, fullRank = T)
  ex <- data.table(predict(oh_model, newdata = dat ))
  data <- union(dat, ex)
  return(data)
}

# Get month and day number  
preprocessing <- function(dat){
  dat$day <- yday(dat$Date)
  dat$month <- month(dat$Date)
  print(dat)
  return(dat)
}


#split the dataset by countries 
countriesdata <- function(data){
 datas <- target(data, targ = "Fatalities")
 datas <- preprocessing(datas)
 datas <- subset(datas, select = -c(sub_region, Date))
 s <-  split(datas, datas$Country_Region)
 return(s)
}

strain <- countriesdata(datrain)
stest <- countriesdata(datest)
stest[2]
strain[1]
z <- data.frame(strain[1])
typeof(z[3:4])
z$Afghanistan.target

########RIDGE ######## 

#split only the train ;)
splitting_train <- function(sdata,tdata){
  #transform the splitted dataset as a data frame
  dat <- data.frame(sdata)
  dattest <- data.frame(tdata)
  # perc splitting
  perc_split <- c(0.79, 0.01, 0.20);
  
  
  # row indices for training data (70%)
  train_index <- sample(1:nrow(dat), perc_split[1]*nrow(dat));  
  
  # row indices for validation data (15%)
  val_index <- sample(setdiff(1:nrow(dat), train_index), perc_split[2]*nrow(dat));  
  
  # row indices for test data (15%)
  test_index <- setdiff(1:nrow(dat), c(train_index, val_index));
  
  # split data
  train <- dat[train_index,]; 
  val <- dat[val_index,]; 
  test  <- dat[test_index,];
  

  #training set to matrix
 xridge <- model.matrix(~.,data= train[3:4])
  lambdas <- 10^seq(2, -3, by = -.1)
  ridge_reg = glmnet(xridge, train[,2], nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

  cv_ridge <- cv.glmnet(xridge,train[,2], alpha = 0, lambda = lambdas)
  optimal_lambda <- cv_ridge$lambda.min
  
  xridget <- model.matrix(~.,data= test[3:4])
  # Prediction and evaluation on train data
  predictions_train <- predict(ridge_reg, newx = xridget, s = optimal_lambda)
  error <- rmse(actual = test[,2],predicted = predictions_train)
  
  # baseline predict with mean 
  baseline <- data.frame(true = test[,2])
  baseline$predict <- round(mean(train[,2]))
  errorbase <-  rmse(actual = test[,2],predicted = baseline$predict)
  
  #predict kaggle values 
  kaggleridge <- model.matrix(~.,data= dattest[3:4])
  prediction_test <- predict(ridge_reg, newx = kaggleridge, s = optimal_lambda)
  Kaggle_ridge <- merge(dattest,prediction_test)
  
  return(list(ridge_reg, optimal_lambda,error,errorbase,Kaggle_ridge))
}

#Training with the first country only
#visuaise responce 
outputridge <- splitting_train(strain[1],stest[1])
typeof(outputridge)
outputridge


#building the test set with more than one country
#the training function is done by country 
count = 1
kaggle_submit <- data.frame(Country_Region=character(0),ForecastId=integer(0),day=integer(0),month=integer(0),target=numeric(0))
for (country in strain){
  outputridge <- splitting_train(country,stest[count])
  outputridge <- data.frame(outputridge[5])
  

  outputridge$Country_Region <- outputridge[,1]
  outputridge$ForecastId <- outputridge[,2]
  outputridge$day <- outputridge[,3]
  outputridge$month <- outputridge[,4]
  outputridge$target <- outputridge[,5]
  ridge <- subset(outputridge, select = c(Country_Region,ForecastId,day,month,target))
  
  kaggle_submit <- union(kaggle_submit,ridge)
  count = count + 1
  if (count ==3){break}
}

kaggle_submit









######## Regressor random FOREST###########


#split only the train ;)
splitting_rf <- function(sdata){
  #transform the splitted dataset as a data frame
  dat <- data.frame(sdata)
  
  #Replca remaining na with 0
  dat[is.na(dat)] <- 0
  
  #dattest <- data.frame(tdata)
  # perc splitting
  perc_split <- c(0.79, 0.01, 0.20);
  
  
  # row indices for training data (70%)
  train_index <- sample(1:nrow(dat), perc_split[1]*nrow(dat));  
  
  # row indices for validation data (15%)
  val_index <- sample(setdiff(1:nrow(dat), train_index), perc_split[2]*nrow(dat));  
  
  # row indices for test data (15%)
  test_index <- setdiff(1:nrow(dat), c(train_index, val_index));
  
  # split data
  train <- dat[train_index,]; 
  val <- dat[val_index,]; 
  test  <- dat[test_index,];
  
  # Set grid ranges
  params <- list(ntree_values=seq(50, 400, by = 50)  ,
                 mtry_values= seq(20, 30, by = 2) ,
                 nodesize_values=seq(3, 9, by = 2)  );
  ### GRID
  grid_results_rf <- foreach (ntree = params$ntree_values, .combine = rbind)%:%
    foreach (mtry = params$mtry_values, .combine = rbind)%:%
    foreach (nodesize = params$nodesize_values, .packages = c("randomForest", "data.table","Metrics"), .combine = rbind)%dopar%{
      
      set.seed(100);
      # Train
      model <- randomForest(train[,3:4], y = train[,2],
                               ntree=ntree, mtry=mtry,nodesize = nodesize,importance = TRUE);
      
      # Predict
      pred_test <- predict(model, newdata = test, type = "response");
      
      
      # Evaluation
      rmse_test <- rmse(actual = test[,2], pred_test);
      
      ### Results
      ret<-data.table(ntree = ntree, mtry = mtry, nodesize = nodesize,
                      rmse_test = rmse_test);
      
    }
  mod <- grid_results_rf[order(rmse_test)];
 
  return(mod)
}


predicting <- function(sdata, tdata) {
  dat <- data.frame(sdata)
  dattest <- data.frame(tdata)
  grid_results_rf <- splitting_rf(dat)
  best <- grid_results_rf[1];

  #Train witht he full dataset this time
  optimal_model <- randomForest(dat[,3:4], y = dat[,2], ntree  = as.integer(best[1,1]), mtry = as.integer(best[1,2]), nodesize = as.integer(best[1,3]))
  pred_kaggle <- predict(optimal_model, newdata = dattest[,3:4], type = "response")
  Kaggle_regression <- merge(dattest, pred_kaggle)
  return(Kaggle_regression)
}


predicting(strain[2],stest[2])



count = 1
kaggle_submit <- data.frame(Country_Region=character(0),ForecastId=integer(0),day=integer(0),month=integer(0),target=numeric(0))
for (country in strain){
  outputrf <- predicting(country,stest[count])
  outputrf <- data.frame(outputrf)
  
  
  outputrf$Country_Region <- outputrf[,1]
  outputrf$ForecastId <- outputrf[,2]
  outputrf$day <- outputrf[,3]
  outputrf$month <- outputrf[,4]
  outputrf$target <- outputrf[,5]
  rf <- subset(outputrf, select = c(Country_Region,ForecastId,day,month,target))
  
  kaggle_submit <- union(kaggle_submit,rf)
  count = count + 1
  if (count ==3){break}
}








optimal_param <- randomForest(train[,3:4], y = train[,2], ntree  = best[1], mtry = best[2], nodesize = best[3])
Kaggle_rf <- merge(dattest,prediction_test)








