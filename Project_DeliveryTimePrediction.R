# food delivery time prediction using Linear Regression model

#Objective: Build a model to predict the delivery time of orders based on other 
#features such as type of vehicle,weather condition,vehicle condition.

# Load necessary libraries


#Read the xl file
library(openxlsx)
# library to generate professional colors
library(RColorBrewer) 
#library to %>%
library(magrittr)
#library for mutate and many other function
library(dplyr)
library(caret)
delivery<-read.xlsx("F:\\Food_Service_Dataset.xlsx")
#delivery<-NULL
#Exploration of food data
head(delivery)
str(delivery)
summary(delivery)
dim(delivery)
names(delivery)
delivery$Road_traffic_density

# Missing values Identification and Treatment
sapply(delivery,function(x)sum(is.na(x)))
numeric_cols <- sapply(delivery, is.numeric)
delivery[numeric_cols] <- apply(delivery[numeric_cols], 2, function(x) as.numeric(gsub("NaN", NA, as.character(x))))
categorical_col <- sapply(delivery, is.character)
delivery[categorical_col] <- apply(delivery[categorical_col], 2, function(x) gsub("NaN", NA, x))
#because these are nan values we can drop them
delivery <- na.omit(delivery)
#we have no na values in any column so we can move forward

################
#Data Preprocessing
########
#Drop unused columns
delivery= delivery[, !(colnames(delivery) %in% c("ID","Time_Orderd","Time_Order_picked","Delivery_person_ID"))]
names(delivery)
delivery$`Time_taken(min)`<-gsub(pattern="\\(min\\) ",replacement ="",x=delivery$`Time_taken(min)`)
delivery$`Time_taken(min)`<-as.numeric(delivery$`Time_taken(min)`)
delivery$Weatherconditions<-gsub(pattern="conditions ",replacement="",x=delivery$Weatherconditions)
head(delivery)

delivery$Delivery_person_Age<-as.numeric(delivery$Delivery_person_Age)
delivery$Delivery_person_Ratings<-as.numeric(delivery$Delivery_person_Ratings)
delivery$multiple_deliveries<-as.numeric(delivery$multiple_deliveries)
table(delivery$multiple_deliveries)
#######
##########
#Feature Engineering
##########
# Compute the distance using the Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371 # Radius of Earth in kilometers
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2) * sin(dlat / 2) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2) * sin(dlon / 2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

delivery <- delivery %>%
  mutate(Distance = haversine_distance(Restaurant_latitude, Restaurant_longitude,
                                       Delivery_location_latitude, Delivery_location_longitude))


head(delivery)
str(delivery)
#order date is numeric type so we can convert it into date type and extract valuable attributes like weekday and month of the delivery
delivery$Order_Date <- as.Date(delivery$Order_Date, origin = "1900-01-01")
delivery$weekday <- format(delivery$Order_Date, "%A")
delivery$month <- format(delivery$Order_Date, "%B")

table(delivery$weekday)
#as we have extracted the distance between the destination and source location we can get rid of latitude and longitude columns
#also order_date
delivery= delivery[, !(colnames(delivery) %in% c("Restaurant_latitude","Restaurant_longitude","Delivery_location_latitude",
                                                 "Delivery_location_longitude","Order_Date"))]

#custom encoding of road_traffic_density
delivery$road_traffic<-delivery$Road_traffic_density
traffic_label <-c("Low " = 0, "Medium " = 1, "High " = 2, "Jam " = 3)
delivery$road_traffic <-traffic_label[delivery$road_traffic]
head(delivery)
table(delivery$road_traffic)
table(delivery$Road_traffic_density)


################
#Exploring Continuous Columns
######
getwd()
#Exploring MULTIPLE CONTINUOUS features
ColHist<-c("Delivery_person_Age","Delivery_person_Ratings","Distance","Vehicle_condition",
           "Time_taken(min)","multiple_deliveries","road_traffic")

#Splitting the plot window into four parts
par(mfrow=c(2,4))


# looping to create the histograms for each column
for (ColumnName in ColHist){
  hist(delivery[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}


table(delivery$Delivery_person_Ratings <3.3)
# values less than 3.0 is 100 out of 41368


##treating outliers for continous columns
#finding nearest value for outliers

max(delivery$Delivery_person_Ratings[delivery$Delivery_person_Ratings<3.3])


##Replacing all values

delivery$Delivery_person_Ratings[delivery$Delivery_person_Ratings < 3.3] <- 3.3

#distance column is giving no normal distribution

############################################################
# Exploring CATEGORICAL feature
table(delivery$Type_of_order)
table(delivery$month)
ColsBar<-c("Type_of_order", "Type_of_vehicle","Weatherconditions","Festival",  "City","weekday", "month")

#Splitting the plot window into four parts
par(mfrow=c(2,4))

# looping to create the Bar-Plots for each column
for (catCol in ColsBar){
  barplot(table(delivery[,c(catCol)]), main=paste('Barplot of:', catCol), 
          col=brewer.pal(8,"Paired"))}
##########################################################
#scatter plot for continuous variable vs predictor variable
######################################################
# For multiple columns at once
colscatter=c("Delivery_person_Age","Delivery_person_Ratings","Distance","Vehicle_condition",
             "Time_taken(min)","multiple_deliveries","road_traffic")
plot(delivery[, colscatter], col='blue')
names(delivery)


#Correlating test between continuous cols and target variable
str(delivery)
ConCols<-c("Delivery_person_Age","Delivery_person_Ratings","Distance","Vehicle_condition",
           "Time_taken(min)","multiple_deliveries","road_traffic")
cor(delivery[, ConCols], use = "complete.obs")


#                          Time_taken(min) 
#Delivery_person_Age         0.297269950     
#Delivery_person_Ratings    -0.363916549             
#Distance                   -0.001078013      
#Vehicle_condition          -0.241701629      
#Time_taken(min)             1.000000000     
#multiple_deliveries         0.385257431     
#road_traffic                0.419287149     

#################################################
#Box plot for categorical columns vs Target
#################################################
boxplot(delivery$`Time_taken(min)` ~ delivery$Type_of_vehicle, data = delivery, col=brewer.pal(8,"Paired"))
boxplot(delivery$`Time_taken(min)` ~ delivery$Type_of_order, data = delivery, col=brewer.pal(8,"Paired"))

ColsForANOVA<-c("Type_of_order", "Type_of_vehicle","Weatherconditions","Festival", 
                "City","weekday", "month") 

for (catCol in ColsForANOVA){
  anovaData= delivery[ , c("Time_taken(min)", catCol)]
  print(str(anovaData))
  print(summary(aov(`Time_taken(min)`~., data= anovaData))[[1]][[5]][1])
  
}
#"Type_of_order"=0.4525879,"Type_of_vehicle"=1.756015e-248,"Weatherconditions"=0, "Festival"=0,  "City"=0,"weekday"=1.496576e-64, "month"=0.05491043
#except Type_of_order all the columns are good for building the model
###################

######################
#Generate df for model
#######################
head(delivery)
str(delivery)
inpdata<-delivery
TargetName<-c("Time_taken(min)")
names(inpdata)
# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis
BestPredictors<-c("Type_of_vehicle","Delivery_person_Age", "Delivery_person_Ratings", "road_traffic",
                   "Weatherconditions","multiple_deliveries","Festival","City","weekday","Vehicle_condition")

# Extracting Target and predictor variables from data to create a generic dataset
TargetVar<-inpdata[ , c(TargetName)]
head(TargetVar)
length(TargetVar)
str(TargetVar)
head(inpdata)

PredictorVar<-inpdata[ , BestPredictors]
str(PredictorVar)
head(PredictorVar)
MLDataNew<-data.frame(TargetVar,PredictorVar)
class(MLDataNew)
str(MLDataNew)
head(MLDataNew)
#converting the categorical variables to factor
MLDataNew$Type_of_vehicle<-as.factor(MLDataNew$Type_of_vehicle)
MLDataNew$Festival<-as.factor(MLDataNew$Festival)
MLDataNew$City<-as.factor(MLDataNew$City)
MLDataNew$weekday<-as.factor(MLDataNew$weekday)
MLDataNew$Weatherconditions<-as.factor(MLDataNew$Weatherconditions)
################################
#ml data splitting 
###############################
set.seed(123)
TrainSampleIndex<-sample(1:nrow(MLDataNew), size=0.7 * nrow(MLDataNew))
DataMLTrain<-MLDataNew[TrainSampleIndex, ]
DataMLTest<-MLDataNew[-TrainSampleIndex, ]
head(DataMLTest)
#

#############################
#fitting the model
#############################
Model_lr<-lm(TargetVar~.,data=DataMLTrain)
summary(Model_lr)


# Residuals:
#   Min       1Q   Median       3Q      Max 
# -19.8898  -4.3780  -0.2694   3.9916  27.1004 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 52.01300    0.65748  79.109  < 2e-16 ***
#   Type_of_vehiclemotorcycle   -0.54858    0.16004  -3.428 0.000609 ***
#   Type_of_vehiclescooter      -1.04053    0.14593  -7.130 1.03e-12 ***
#   Delivery_person_Age          0.37326    0.00642  58.142  < 2e-16 ***
#   Delivery_person_Ratings     -7.83300    0.12185 -64.285  < 2e-16 ***
#   road_traffic                 2.53312    0.03006  84.260  < 2e-16 ***
#   WeatherconditionsFog         0.08913    0.12513   0.712 0.476271    
# WeatherconditionsSandstorms -2.77398    0.12681 -21.875  < 2e-16 ***
#   WeatherconditionsStormy     -2.76150    0.12602 -21.912  < 2e-16 ***
#   WeatherconditionsSunny      -6.10761    0.12775 -47.808  < 2e-16 ***
#   WeatherconditionsWindy      -2.67360    0.12632 -21.165  < 2e-16 ***
#   multiple_deliveries          3.21453    0.06653  48.314  < 2e-16 ***
#   FestivalYes                  9.89517    0.26675  37.095  < 2e-16 ***
#   CitySemi-Urban               9.65150    0.60225  16.026  < 2e-16 ***
#   CityUrban                   -2.13445    0.08780 -24.311  < 2e-16 ***
#   weekdayMonday               -0.94402    0.13516  -6.984 2.92e-12 ***
#   weekdaySaturday             -1.32241    0.13431  -9.846  < 2e-16 ***
#   weekdaySunday               -0.36518    0.13100  -2.788 0.005314 ** 
#   weekdayThursday             -1.32664    0.13480  -9.841  < 2e-16 ***
#   weekdayTuesday              -0.74203    0.13531  -5.484 4.19e-08 ***
#   weekdayWednesday            -0.69661    0.13504  -5.159 2.50e-07 ***
#   Vehicle_condition           -2.17325    0.05851 -37.141  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.208 on 28935 degrees of freedom
# Multiple R-squared:  0.5598,	Adjusted R-squared:  0.5594 
# F-statistic:  1752 on 21 and 28935 DF,  p-value: < 2.2e-16



# Looking at the distribution of residuals
colnames(DataMLTrain)
hist(Model_lr$residuals)



#Checking Multicollinearity in the model
## Get the predicted or fitted values
library("car")
vif(Model_lr)

# GVIF Df GVIF^(1/(2*Df))
# Type_of_vehicle         1.698903  2        1.141674
# Delivery_person_Age     1.033022  1        1.016377
# Delivery_person_Ratings 1.044511  1        1.022013
# road_traffic            1.051030  1        1.025198
# Weatherconditions       1.023657  5        1.002341
# multiple_deliveries     1.091898  1        1.044939
# Festival                1.054619  1        1.026946
# City                    1.039305  2        1.009685
# weekday                 1.007803  6        1.000648
# Vehicle_condition       1.717782  1        1.310642


Model_lr2<-lm(TargetVar~.-Weatherconditions,data=DataMLTrain)
summary(Model_lr2)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -20.4026  -4.5307  -0.4146   4.1060  26.0616 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                49.602862   0.683231  72.600  < 2e-16 ***
#   Type_of_vehiclemotorcycle  -0.581760   0.168575  -3.451 0.000559 ***
#   Type_of_vehiclescooter     -1.035643   0.153728  -6.737 1.65e-11 ***
#   Delivery_person_Age         0.362005   0.006759  53.559  < 2e-16 ***
#   Delivery_person_Ratings    -7.787845   0.127973 -60.855  < 2e-16 ***
#   road_traffic                2.495616   0.031658  78.830  < 2e-16 ***
#   multiple_deliveries         3.539142   0.069827  50.685  < 2e-16 ***
#   FestivalYes                10.675923   0.280451  38.067  < 2e-16 ***
#   CitySemi-Urban              9.801089   0.634409  15.449  < 2e-16 ***
#   CityUrban                  -2.352205   0.092396 -25.458  < 2e-16 ***
#   weekdayMonday              -0.864349   0.142373  -6.071 1.29e-09 ***
#   weekdaySaturday            -1.300584   0.141491  -9.192  < 2e-16 ***
#   weekdaySunday              -0.359377   0.138007  -2.604 0.009218 ** 
#   weekdayThursday            -1.331783   0.142007  -9.378  < 2e-16 ***
#   weekdayTuesday             -0.735889   0.142539  -5.163 2.45e-07 ***
#   weekdayWednesday           -0.671679   0.142239  -4.722 2.34e-06 ***
#   Vehicle_condition          -2.128764   0.061632 -34.540  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.54 on 28940 degrees of freedom
# Multiple R-squared:  0.5113,	Adjusted R-squared:  0.5111 
# F-statistic:  1893 on 16 and 28940 DF,  p-value: < 2.2e-16


vif(Model_lr2)

# GVIF Df GVIF^(1/(2*Df))
# Type_of_vehicle         1.698281  2        1.141570
# Delivery_person_Age     1.031781  1        1.015766
# Delivery_person_Ratings 1.038151  1        1.018897
# road_traffic            1.050220  1        1.024802
# multiple_deliveries     1.083671  1        1.040995
# Festival                1.050382  1        1.024881
# City                    1.037033  2        1.009132
# weekday                 1.006918  6        1.000575
# Vehicle_condition       1.717235  1        1.310433

## Get the predicted or fitted values
fitted(Model_lr2)
par(mfrow=c(2,2))
plot(Model_lr2)

## MAPE
DataMLTrain$Model_lrpred <-fitted(Model_lr2)


#Calculating MAPE on training data
library(MLmetrics)
MAPE(DataMLTrain$TargetVar,DataMLTrain$Model_lrpred)
#0.2041423
AIC(Model_lr2)
#190957.4
BIC(Model_lr2)
#191106.3

library(car)
# Checking Autocorrelation
durbinWatsonTest(Model_lr2)
# lag Autocorrelation D-W Statistic p-value
# 1   -0.0007255576      2.001408     0.9
# Alternative hypothesis: rho != 0


# Finally Predict on test data
DataMLTest$pred <-  predict(Model_lr2,DataMLTest[,-1])

# Checking MAPE on test data
MAPE(DataMLTest$TargetVar,DataMLTest$pred)
#0.2038168
# Checking R2, RMSE and MAE on test data
accuracyML<-data.frame(
  R2 = R2(DataMLTest$pred, DataMLTest$TargetVar),
  RMSE = RMSE(DataMLTest$pred,DataMLTest$TargetVar) ,
  MAE = MAE(DataMLTest$pred, DataMLTest$TargetVar)
)
print(paste(accuracyML))
#"0.506146343762689" "6.52850442338871"  "5.16896538482406" 
########################################################################
###### Decision Tree #######
#install.packages('party')
str(DataMLTest)
str(DataMLTrain)
DataMLTrain <- DataMLTrain[ , !(names(DataMLTrain) %in% c("Model_lrpred"))]
DataMLTest <- DataMLTest[ , !(names(DataMLTest) %in% c("pred"))]
library(party)
startTime=Sys.time()
Model_CTREE=ctree(TargetVar ~ . , data=DataMLTrain, controls =ctree_control(maxdepth=5))
plot(Model_CTREE)
endTime=Sys.time()
endTime-startTime

# Checking Accuracy of model on Testing data
DataMLTest$Pred_CTREE=as.numeric(predict(Model_CTREE, DataMLTest[-1]))
head(DataMLTest)

# Calculating the Absolute Percentage Error for each prediction
CTREE_APE= 100 *(abs(DataMLTest$Pred_CTREE - DataMLTest$TargetVar)/DataMLTest$TargetVar)
print(paste('### Mean Accuracy of ctree Model is: ', 100 - mean(CTREE_APE)))
print(paste('### Median Accuracy of ctree  Model is: ', 100 - median(CTREE_APE)))
"### Mean Accuracy of ctree Model is:  80.570984266853"

"### Median Accuracy of ctree  Model is:  86.4179556466642"




##################################################################

#Polynomial Regression

##################################################################
head(DataMLTest)
str(DataMLTrain)
DataMLTest <- DataMLTest[ , !(names(DataMLTest) %in% c("Pred_CTREE"))]
names(DataMLTrain)
poly_reg<-lm(TargetVar ~ Type_of_vehicle + poly(Delivery_person_Age, 2) + 
               poly(Delivery_person_Ratings, 2) + poly(road_traffic, 2) +
               poly(multiple_deliveries, 2) +poly(Vehicle_condition,2)+ Festival + City + weekday,
             data = DataMLTrain)



summary(poly_reg)

# 
#                                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         27.54456    0.17124 160.858  < 2e-16 ***
#   Type_of_vehiclemotorcycle           -0.02429    0.16438  -0.148  0.88252    
# Type_of_vehiclescooter               0.04119    0.15395   0.268  0.78906    
# poly(Delivery_person_Age, 2)1      356.79065    6.42923  55.495  < 2e-16 ***
#   poly(Delivery_person_Age, 2)2       -0.95421    6.32857  -0.151  0.88015    
# poly(Delivery_person_Ratings, 2)1 -403.27146    6.45973 -62.429  < 2e-16 ***
#   poly(Delivery_person_Ratings, 2)2  118.15400    6.37064  18.547  < 2e-16 ***
#   poly(road_traffic, 2)1             530.36080    6.50332  81.552  < 2e-16 ***
#   poly(road_traffic, 2)2            -131.24271    6.35142 -20.664  < 2e-16 ***
#   poly(multiple_deliveries, 2)1      335.70683    6.60890  50.796  < 2e-16 ***
#   poly(multiple_deliveries, 2)2      142.94614    6.48080  22.057  < 2e-16 ***
#   poly(Vehicle_condition, 2)1       -310.37834    8.31970 -37.306  < 2e-16 ***
#   poly(Vehicle_condition, 2)2        186.78062    6.65456  28.068  < 2e-16 ***
#   FestivalYes                          9.46502    0.27456  34.473  < 2e-16 ***
#   CitySemi-Urban                       8.15111    0.61756  13.199  < 2e-16 ***
#   CityUrban                           -2.20144    0.08947 -24.605  < 2e-16 ***
#   weekdayMonday                       -0.85062    0.13776  -6.175 6.72e-10 ***
#   weekdaySaturday                     -1.27211    0.13696  -9.288  < 2e-16 ***
#   weekdaySunday                       -0.38066    0.13352  -2.851  0.00436 ** 
#   weekdayThursday                     -1.32851    0.13744  -9.666  < 2e-16 ***
#   weekdayTuesday                      -0.66967    0.13792  -4.855 1.21e-06 ***
#   weekdayWednesday                    -0.68607    0.13763  -4.985 6.24e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.327 on 28935 degrees of freedom
# Multiple R-squared:  0.5427,	Adjusted R-squared:  0.5424 
# F-statistic:  1635 on 21 and 28935 DF,  p-value: < 2.2e-16
# Looking at the distribution of residuals

poly_reg2<-lm(TargetVar ~ poly(Delivery_person_Ratings, 2) + poly(road_traffic, 2) +
               poly(multiple_deliveries, 2) +poly(Vehicle_condition,2)+ Festival + City + weekday,
             data = DataMLTrain)



summary(poly_reg2)


# Residuals:
#   Min      1Q  Median      3Q     Max 
# -30.204  -4.580  -0.299   4.253  25.580 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         27.52348    0.10152 271.115  < 2e-16 ***
#   poly(Delivery_person_Ratings, 2)1 -441.43197    6.75529 -65.346  < 2e-16 ***
#   poly(Delivery_person_Ratings, 2)2  116.92798    6.69989  17.452  < 2e-16 ***
#   poly(road_traffic, 2)1             518.19958    6.83619  75.802  < 2e-16 ***
#   poly(road_traffic, 2)2            -134.25284    6.68016 -20.097  < 2e-16 ***
#   poly(multiple_deliveries, 2)1      371.43948    6.91789  53.693  < 2e-16 ***
#   poly(multiple_deliveries, 2)2      147.77663    6.81575  21.682  < 2e-16 ***
#   poly(Vehicle_condition, 2)1       -300.47088    6.75491 -44.482  < 2e-16 ***
#   poly(Vehicle_condition, 2)2        178.85546    6.69195  26.727  < 2e-16 ***
#   FestivalYes                         10.02240    0.28858  34.730  < 2e-16 ***
#   CitySemi-Urban                       8.75681    0.64945  13.483  < 2e-16 ***
#   CityUrban                           -2.47515    0.09396 -26.342  < 2e-16 ***
#   weekdayMonday                       -0.74786    0.14488  -5.162 2.46e-07 ***
#   weekdaySaturday                     -1.17115    0.14404  -8.131 4.44e-16 ***
#   weekdaySunday                       -0.32264    0.14043  -2.298 0.021593 *  
#   weekdayThursday                     -1.25793    0.14455  -8.702  < 2e-16 ***
#   weekdayTuesday                      -0.55295    0.14505  -3.812 0.000138 ***
#   weekdayWednesday                    -0.63947    0.14475  -4.418 1.00e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.655 on 28939 degrees of freedom
# Multiple R-squared:  0.4941,	Adjusted R-squared:  0.4938 
# F-statistic:  1662 on 17 and 28939 DF,  p-value: < 2.2e-16

hist(poly_reg2$residuals)


#Checking Multicollinearity in the model
## Get the predicted or fitted values
library("car")
vif(poly_reg2)

#                                       GVIF Df GVIF^(1/(2*Df))
# poly(Delivery_person_Ratings, 2) 1.044219  2        1.010876
# poly(road_traffic, 2)            1.063091  2        1.015413
# poly(multiple_deliveries, 2)     1.130636  2        1.031171
# poly(Vehicle_condition, 2)       1.041308  2        1.010171
# Festival                         1.074136  1        1.036405
# City                             1.048378  2        1.011881
# weekday                          1.007901  6        1.000656

## Get the predicted or fitted values
fitted(poly_reg2)
par(mfrow=c(2,2))
plot(poly_reg2)

## MAPE
DataMLTrain$poly_pred <-fitted(poly_reg2)


#Calculating MAPE on training data
library(MLmetrics)
MAPE(DataMLTrain$TargetVar,DataMLTrain$poly_pred)
#0.2062743
AIC(poly_reg2)
#191966
BIC(poly_reg2)
#192123.2
# Checking Autocorrelation
durbinWatsonTest(poly_reg2)
# lag Autocorrelation D-W Statistic p-value
# 1    -0.007046657      2.014035   0.236
# Alternative hypothesis: rho != 0

# Finally Predict on test data
DataMLTest$polypred <- predict(poly_reg2,DataMLTest[,-1])

# Checking MAPE on test data
MAPE(DataMLTest$TargetVar,DataMLTest$polypred)
#0.2059452
library(caret)
# Checking R2, RMSE and MAE on test data
Modelaccuracy<-data.frame(
  R2 = R2(DataMLTest$polypred, DataMLTest$TargetVar),
  RMSE = RMSE(DataMLTest$polypred,DataMLTest$TargetVar) ,
  MAE = MAE(DataMLTest$polypred, DataMLTest$TargetVar)
)
print(paste(Modelaccuracy))
#"0.490580334805314" "6.63059900128957"  "5.24133415675625" 




