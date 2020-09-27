rm(list=ls()) 

# set working directory
setwd("C:/Users/Guest/Desktop/harsh jain/Project -2")
getwd()

# load all libraries
 x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50",
                    "dummies", "e1071", "Information","MASS", "rpart", "gbm", "ROSE", 'sampling'
                    , 'DataCombine', 'inTrees','usdm')

lapply(x, require, character.only = TRUE)
rm(x)

# loading the data
train=read.csv("train_cab.csv",header=TRUE)
test=read.csv("test.csv",header=TRUE)

# EXPLORING DATA
#viewing the data
head(train,5) 
head(test,5)

#structure of data or data types
str(train)
str(test)

#Summary of data
summary(train)
summary(test)

#dimension of data
dim(train)
dim(test)

#unique value of each count
apply(train, 2,function(x) length(table(x)))
apply(test, 2,function(x) length(table(x)))

# converting the features in the required data types.
train$fare_amount = as.numeric(as.character(train$fare_amount))
train$passenger_count=round(train$passenger_count)

# DATA CLEANING & OUTLIER 
# remove irregular data point in data set
sort(train$fare_amount,decreasing = TRUE)

# fare amount cannot be less than one 
# considring fare amount 453 as max 
# removing all the fare amount greater than 453 and less than 1.

# count no. of row which is outside the condition
nrow(train[which(train$fare_amount < 1 ),])
nrow(train[which(train$fare_amount >453 ),])

#remove those data point in fare_amount
train = train[-which(train$fare_amount < 1 ),] 
train = train[-which(train$fare_amount >453 ),]

sort(train$passenger_count,decreasing = TRUE)
# passenger count cannot be Zero
# even if we consider suv max seat is 6
# so removing passenger count greater than 6 and less than 1.
# count no. of row which is outside the condition
nrow(train[which(train$passenger_count < 1 ),])
nrow(train[which(train$passenger_count >6 ),])

#remove those data point in passengger_count
train=train[-which(train$passenger_count < 1 ),]
train=train[-which(train$passenger_count >6 ),]

# Latitudes range from -90 to 90.
# Longitudes range from -180 to 180.
# Removing which does not satisfy these ranges.

print(paste('pickup_longitude above 180=',nrow(train[which(train$pickup_longitude >180 ),])))
print(paste('pickup_longitude above -180=',nrow(train[which(train$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90=',nrow(train[which(train$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90=',nrow(train[which(train$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180=',nrow(train[which(train$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180=',nrow(train[which(train$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90=',nrow(train[which(train$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90=',nrow(train[which(train$dropoff_latitude > 90 ),])))

# removing one data point
train = train[-which(train$pickup_latitude > 90),] 

# Also we will see if there are any values equal to 0.
nrow(train[which(train$pickup_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])
nrow(train[which(train$dropoff_longitude == 0 ),])
nrow(train[which(train$dropoff_latitude == 0 ),])

# removing those data points.
train=train[-which(train$pickup_longitude == 0 ),]
train=train[-which(train$dropoff_longitude == 0),]

# calculate distance from the given coordinates of latitude and longitude.
deg_to_rad = function(deg){
  (deg * pi) / 180
}
haversine = function(long1,lat1,long2,lat2){
  
  #long1rad = deg_to_rad(long1)
  phi1 = deg_to_rad(lat1)
  
  #long2rad = deg_to_rad(long2)
  phi2 = deg_to_rad(lat2)
  
  delphi = deg_to_rad(lat2 - lat1)
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  R = 6371e3
  R * c / 1000 
}


train$distance = haversine(train$pickup_longitude,train$pickup_latitude,train$dropoff_longitude,train$dropoff_latitude)
test$distance = haversine(test$pickup_longitude,test$pickup_latitude,test$dropoff_longitude,test$dropoff_latitude)

# distance variable is obtained from co-ordinates points ,so there is no further use of pickup & dropoff points.
# hence we drop these varible from data set
train = subset(train,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
test = subset(test,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

# considering the distance 130 as max and greater than 0,considering rest as outlier.
nrow(train[which(train$distance ==0 ),])
nrow(test[which(test$distance==0 ),])
nrow(train[which(train$distance >130 ),])
nrow(test[which(test$distance >130 ),])

# removing the data points by considering the above conditions.
train=train[-which(train$distance ==0 ),]
train=train[-which(train$distance >130 ),]
test=test[-which(test$distance ==0 ),]

str(train)
str(test)

summary(train)
summary(test)

# Convert pickup_datetime from factor to date time
# new features will be year,month,date,hour

train$pickup_datetime = as.POSIXct(train$pickup_datetime,format='%Y-%m-%d %H:%M:%S UTC')
train$year =as.integer(format(train$pickup_datetime,"%Y"))
train$month =as.integer(format(train$pickup_datetime,"%m"))
train$date = as.integer(format(train$pickup_datetime,"%d"))
train$hour = as.integer(format(train$pickup_datetime,"%H"))

# for test data set.
test$pickup_datetime = as.POSIXct(test$pickup_datetime,format='%Y-%m-%d %H:%M:%S UTC')
test$year =as.integer(format(test$pickup_datetime,"%Y"))
test$month =as.integer(format(test$pickup_datetime,"%m"))
test$date = as.integer(format(test$pickup_datetime,"%d"))
test$hour = as.integer(format(test$pickup_datetime,"%H"))
 
#Drop the variable Pickup-datetime
train = subset(train,select = -c(pickup_datetime))
test = subset(test,select = -c(pickup_datetime))

str(train)
str(test)

# MISSINNG VALUE ANALYSIS
# checking missing data
apply(train,2, function(x) {sum(is.na(x))})

#Creating dataframe with missing values present in each variable
miss_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))
miss_val$Columns = row.names(miss_val)
row.names(miss_val) = NULL
names(miss_val)[1] = "miss_percentage"

#Calculating percentage missing value
miss_val$miss_percentage = (miss_val$miss_percentage/nrow(train )) * 100

# Sorting null_val in Descending order
miss_val = miss_val[order(-miss_val$miss_percentage),]

# Reordering columns
miss_val = miss_val[,c(2,1)]

#We have seen that null values are very less in our data set i.e. less than 1%.
#So we can delete the columns having missing values
train =DropNA(train)

#Verifying missing values after deletion
sum(is.na(train))
names(train)

# OUTLIER ANALYSIS = I DID IT MANNULLY IN ABOVE CODING #

# FEATURE SELECTION
#selecting only numeric
numeric_index = sapply(train,is.numeric) 
numeric_data = train[,numeric_index]
cnames = colnames(numeric_data)

#Correlation analysis for numeric variables
corrgram(train[,numeric_index],upper.panel=panel.pie, main = "Correlation Plot")

# hence all variable are important.

# FEATURE SCALING
truehist(train$fare_amount)
lines(density(train$fare_amount))

d=density(train$fare_amount)
plot(d,main="distribution")
polygon(d,col="green",border="red")

D=density(train$distance)
plot(D,main="distribution")
polygon(D,col="yellow",border = "red")

A=density(test$distance)
plot(A,main="distribution")
polygon(A,col="black",border="red")

#Normalisation
# log transformation.
train$fare_amount=log1p(train$fare_amount)
test$distance=log1p(test$distance)
train$distance=log1p(train$distance)

# checking back features after transformation.
d=density(train$fare_amount)
plot(d,main="distribution")
polygon(d,col="green",border="red")

D=density(train$distance)
plot(D,main="distribution")
polygon(D,col="red",border="black")

A=density(test$distance)
plot(A,main="distribution")
polygon(A,col="black",border="red")

###check multicollearity
vif(train[,-1])
vifcor(train[,-1], th = 0.8)

#No variable from the 6 input variables has collinearity problem. 
#The linear correlation coefficients ranges between: 
#min correlation ( distance ~ passenger_count ):  0.001499388
#max correlation ( month ~ year ):  -0.124351 

#--------- VIFs of the remained variables -------- 
# Variables      VIF
#1 passenger_count 1.001601
#2        distance 1.001827
#3            year 1.017753
#4           month 1.017362
#5            date 1.002016
#6            hour 1.002634

# MODEL BUILDING
#Divide data into trainset and testset
set.seed(1234)
Train.index = sample(1:nrow(train), 0.8 * nrow(train))
Train_set =  train[ Train.index,]
Test_set =  train[-Train.index,]

## accuracy check
#defining the function (to find the error percentage)
mape=function(av,pv){
  mean(abs((av-pv)/av))*100 
}
#av = actual value
#pv = predicted value

# LINEAR REGRESSION #

#Develop Model on training data
linear_model=lm(fare_amount~.,data=Train_set)
summary(linear_model)

#Lets predict for testset data
predict_lm=predict(linear_model,Test_set[,2:7])
# For test data
predict_test=predict(linear_model,test)

# model evalution
mape(Test_set[,1],predict_lm)
# 7.500
regr.eval(Test_set[,1],predict_lm)
#        mae        mse       rmse       mape 
#  0.17297582 0.07128912 0.26700023 0.07500190 

# DECISION TREE #

#Develop Model on training data
DT=rpart(fare_amount~.,data=Train_set,method="anova")

#Lets predict for testset data
predictions_tree=predict(DT,Test_set[,2:7])

# For test data
predictions_test=predict(DT,test)

# model evalution
mape(Test_set[,1],predictions_tree)
# 8.09
regr.eval(Test_set[,1],predictions_tree)
#       mae        mse       rmse       mape 
#   0.18633188 0.07226311 0.26881799 0.0809881

# RANDOM FOREST

#Develop Model on training data
rf_model = randomForest(fare_amount~ ., Train_set, importance = TRUE, ntree = 500)

#Extract rules fromn random forest
treeList = RF2List(rf_model) 

#Extract rules
rules= extractRules(treeList, Train_set[,2:7])

#Visualize some rules
rules[1:2,]

#Make rules more readable:
readrules = presentRules(rules, colnames(Train_set))
readrules[1:2,]

#Lets predict for testset data
RF_Predictions = predict(rf_model, Test_set[,2:7])

# For test data
RF_test = predict(rf_model, test)

#model evalution
mape(Test_set[,1],RF_Predictions)
# 7.19

regr.eval(Test_set[,1],RF_Predictions)
#       mae        mse       rmse       mape 
#   0.16310940 0.05805191 0.24093963 0.07192287 

# from above result it state that random forest is best model 
# And accuracy of model in order of - Random forest > Linear regression > Decision tree

# Hence we use random forest model for this data set:- 
test$predicted_fare = with(test,RF_test)

# save this predicted value in data set
write.csv(test,'Predicted_value_in_R.csv',row.names = FALSE)

## END OF PROJECT ##
