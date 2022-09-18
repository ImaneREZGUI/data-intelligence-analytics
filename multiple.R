library(readr)
multiple <- read_delim("R/TUV/multiple.csv", 
                       +     delim = ";", escape_double = FALSE, col_types = cols(pressure = col_number()), 
                       +     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                       +     trim_ws = TRUE)
View(multiple)
library(FactoMineR)
PCA(multiple)
summary(PCA(multiple))

#---------------------------------------
#---------------------------------------

#linear model
model1<-lm(yield~amount+duration+pressure+temperature1, multiple)
summary(model1)
#since the coefficient for duration has not  signification, I took it out and this enhance little bit the model  
model11<-lm(yield~amount+pressure+temperature1, multiple)
summary(model1)

#---------------------------------------
#---------------------------------------

# We split the data into training and testing. 80% for training, 20% for testing.
set.seed(123)

samp <- sample(nrow(multiple), 0.8 * nrow(multiple))

train <- multiple[samp, ]

test <- multiple[-samp, ]

#---------------------------------------
#---------------------------------------
#Random forest model

library(randomForest)

model2 <- randomForest(formula = yield ~ .,data = train, ntree = 1000, mtry = 5)

#---------------------
#-----------------------

#gbm model
library(gbm)
library(MASS)
model3<- gbm(yield ~., data = multiple)



#comparison: since some of the algorithms use training and testing data
# I choose the  sum of the square of the error for the test data as a criteria
# The model that got the smallest "Residuals", is the better ( best)..


predection1<-predict.lm(model1, newdata = test)
prediction2 <- predict(model2, newdata = test)
prediction3<-predict(model3,newdata = test)


Residuals1<-sum((predection1-test$yield)*(predection1-test$yield))
Residuals2<-sum((prediction2-test$yield)*(prediction2-test$yield))
Residuals3<-sum((prediction3-test$yield)*(prediction3-test$yield))


Residuals1;Residuals2;Residuals3
