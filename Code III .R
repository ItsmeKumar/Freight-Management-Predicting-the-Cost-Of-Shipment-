rm(list = ls(all=T))

setwd("~/Downloads/project")

# Convert to factors, and nullify the columns which aren't required
data<-read.csv("project_fnl_data1.csv",sep=",",header=T)
data$weight_bins<-NULL
data$ORDER_NBR<-NULL
data$FIRST_PICK_ZIP<-NULL
data$FIRST_PICK_EARLY_APPT<-NULL
data$FIRST_PICK_LATE_APPT<-NULL
data$LAST_DELIVERY_EARLY_APPT<-NULL
data$LAST_DELIVERY_ZIP<-NULL
data$LAST_DELIVERY_LATE_APPT<-NULL
data$CREATED_DATE<-NULL
data$WEIGHT<-NULL
data$SRC_DST_MTCH<-as.factor(data$SRC_DST_MTCH)
data$weight_bins<-as.factor(data$weight_bins)
data$SRC_DST_MTCH<-as.factor(data$SRC_DST_MTCH)
x<-data$WEIGHT

# Take the KMeans, with four centers
km = kmeans(x, centers=4,
            iter.max=20)
data$weight_bins<-km$cluster
numeric_Variables = data[,c(2,7)]

# Compute the order cost
library(vegan)
numeric_Variables = decostand(numeric_Variables,"range")
target_variable = subset(data,select="ORDER_COST")
catDummies <- model.matrix(data$ORDER_COST ~ data$EQUIPMENT_TYPE+
                            data$SOURCE_REGION
                           +data$DESTINATION_REGION
                           +data$IS_HAZARDOUS
                           +data$DELIVERY_LATE_DAYOFWEEK
                           +data$SRC_DST_MTCH
                           +data$weight_bins)[,-1]

# Perofm Linear regression
rows=seq(1,nrow(data),1)
set.seed(1237482)
trainRows=sample(rows,(70*nrow(data))/100)
train1 = data.frame(numeric_Variables, catDummies,ORDER_COST=data$ORDER_COST)[trainRows,]
test1 = data.frame(numeric_Variables, catDummies,ORDER_COST=data$ORDER_COST)[-trainRows,]
options(scipen=16)
LinReg<- lm(ORDER_COST ~ ., data=train1)
summary(LinReg)

# Evaluate the efficincy & accuracy
library(DMwR)
library(MASS)
library(DMwR)
lmtrain = regr.eval(train1$ORDER_COST, predict(LinReg,train1))
lmtest = regr.eval(test1$ORDER_COST, predict(LinReg,test1))
lmtrain
lmtest

#Converted the data into matrix form to input into glm model
##################################################################
data2 <- as.matrix(data.frame(numeric_Variables, catDummies))
train = data2[trainRows,] 
test = data2[-trainRows,]

#Target Varaible
y=data$ORDER_COST[trainRows]
ytest = data$ORDER_COST[-trainRows]
library(glmnet)
#####################################################
fit1=glmnet(train,y,alpha=1)  #Lasso
plot(fit1,xvar="lambda",label=TRUE)

fit2=glmnet(train,y,alpha=0)  #Ridge
plot(fit2,xvar="lambda",label=TRUE)

#######################################################

#cv.glmnet will help you choose lambda
cv <- cv.glmnet(train,y)

#lambda.min - value of lambda that gives minimum cvm - mean cross-validated error
###################################################
# Lasso Regression  using glmnet - L1 norm
fit1=glmnet(train,y,lambda=cv$lambda.min,alpha=1)

predict(fit1,train)

# Accuracy of lasso regression
library(DMwR)
LASSOtrain = regr.eval(y, predict(fit1,train))
LASSOtest = regr.eval(ytest, predict(fit1,test))
LASSOtrain
LASSOtest

# Coefficient plot
coef(fit1)
cv.lasso=cv.glmnet(train,y)
plot(cv.lasso)
coef(cv.lasso)

#############################################################################
# Ridge Regression  using glmnet  - L2 norm
library(glmnet)

# fit model
fit2=glmnet(train,y,lambda=cv$lambda.min,alpha=0)
predict(fit2,train)

library(DMwR)
RIDGEtrain = regr.eval(y, predict(fit2,train))
RIDGEtest = regr.eval(ytest, predict(fit2,test))
RIDGEtrain
RIDGEtest

#Model Selection
coef(fit2) 
cv.ridge=cv.glmnet(train,y,alpha=0)
plot(cv.ridge)
coef(cv.ridge)


# summarize the fit
fit3=glmnet(train,y,lambda=cv$lambda.min,alpha=0.05)
summary(fit3)
predict(fit3,train)

# Accuracy of elastic training
library(DMwR)
Elastictrain = regr.eval(y, predict(fit3,train))
Elastictest = regr.eval(ytest, predict(fit3,test))
Elastictrain
Elastictest

# make predictions
predictions <- predict(fit3, train, type="link")

# summarize accuracy
rmse <- rmse(actual = y, predicted = predictions)
print(rmse)

#################################################
lmtrain
lmtest
LASSOtrain
LASSOtest
RIDGEtrain
RIDGEtest
Elastictrain
Elastictest

# Summarize the final errors
finalerros1 <- data.frame(rbind(lmtrain,lmtest,
                               LASSOtrain,LASSOtest,
                               RIDGEtrain,RIDGEtest,
                               Elastictrain,Elastictest))
finalerros1
