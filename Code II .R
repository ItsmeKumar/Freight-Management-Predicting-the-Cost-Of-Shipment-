rm(list = ls(all=T))

setwd("~/Downloads/project")

# Read the pre processed data, and nullify the unrequired columns
data<-read.csv("project_fnl_data1.csv",sep=",",header=T)
data$ORDER_NBR<-NULL
data$FIRST_PICK_ZIP<-NULL
data$FIRST_PICK_EARLY_APPT<-NULL
data$FIRST_PICK_LATE_APPT<-NULL
data$LAST_DELIVERY_EARLY_APPT<-NULL
data$LAST_DELIVERY_ZIP<-NULL
data$LAST_DELIVERY_LATE_APPT<-NULL
data$CREATED_DATE<-NULL

# Convert columns to factors wherever required
data$SRC_DST_MTCH<-as.factor(data$SRC_DST_MTCH)
data$weight_bins<-as.factor(data$weight_bins)

# 
# Dummify the vars, and predict the 
#
library(caret)
dummies <- predict(dummyVars(~ IS_HAZARDOUS, data = data), newdata = data)
head(dummies, n = 3)
str(dummies)
data<-cbind(data,dummies)
data$IS_HAZARDOUS<-NULL
str(data)

dummies <- predict(dummyVars(~ EQUIPMENT_TYPE, data = data), newdata = data)
head(dummies, n = 3)
data<-cbind(data,dummies)
data$EQUIPMENT_TYPE<-NULL
str(data)

dummies <- predict(dummyVars(~ SOURCE_REGION, data = data), newdata = data)
head(dummies, n = 3)
data<-cbind(data,dummies)
data$SOURCE_REGION<-NULL
str(data)

dummies <- predict(dummyVars(~ DESTINATION_REGION, data = data), newdata = data)
head(dummies, n = 3)
data<-cbind(data,dummies)
data$DESTINATION_REGION<-NULL

dummies <- predict(dummyVars(~ DELIVERY_LATE_DAYOFWEEK, data = data), newdata = data)
head(dummies, n = 3)
data<-cbind(data,dummies)
data$DELIVERY_LATE_DAYOFWEEK<-NULL
str(data)

dummies <- predict(dummyVars(~ SRC_DST_MTCH, data = data), newdata = data)
head(dummies, n = 3)
data<-cbind(data,dummies)
data$SRC_DST_MTCH<-NULL
dummies <- predict(dummyVars(~ weight_bins, data = data), newdata = data)
head(dummies, n = 3)
data<-cbind(data,dummies)
data$weight_bins<-NULL
str(data)

## as we have binned the weight into 4 groups ... we will remove WEIGHTS from the dataset.
data$WEIGHT<-NULL
names(data)
targt<-data$ORDER_COST
data$ORDER_COST<-NULL
data$ORDER_COST<-targt
names(data)<-c("CUSTOMER_MILES",                            
 "time",
 "IS_HAZARDOUS.N" ,
 "IS_HAZARDOUS.Y" ,
 "EQUIPMENT_TYPE.DRY.FREIGHT" ,
 "EQUIPMENT_TYPE.FLATBED" ,
 "EQUIPMENT_TYPE.REFRIGERATED",          
"SOURCE_REGION.California" ,
"SOURCE_REGION.Carolinas",              
 "SOURCE_REGION.Florida.So.Georgia" ,
"SOURCE_REGION.Great.Lakes"  ,          
"SOURCE_REGION.Lower.Atlantic"  ,
"SOURCE_REGION.Lower.Midwest"  ,        
 "SOURCE_REGION.Lower.Mountain" ,
"SOURCE_REGION.New.England" ,           
"SOURCE_REGION.Ohio.River",
"SOURCE_REGION.Pacific.Northwest" ,     
"SOURCE_REGION.South.Central",
"SOURCE_REGION.Southeast" ,             
"SOURCE_REGION.Upper.Atlantic" ,
"SOURCE_REGION.Upper.Midwest",
"SOURCE_REGION.Upper.Mountain" ,
"DESTINATION_REGION.California" ,       
"DESTINATION_REGION.Carolinas" ,
"DESTINATION_REGION.Florida.So.Georgia",
"DESTINATION_REGION.Great.Lakes" ,
"DESTINATION_REGION.Lower.Atlantic"  ,  
"DESTINATION_REGION.Lower.Midwest",
"DESTINATION_REGION.Lower.Mountain" ,   
"DESTINATION_REGION.New.England" ,
"DESTINATION_REGION.Ohio.River",        
"DESTINATION_REGION.Pacific.Northwest",
"DESTINATION_REGION.South.Central"  ,   
"DESTINATION_REGION.Southeast",
"DESTINATION_REGION.Upper.Atlantic" ,   
"DESTINATION_REGION.Upper.Midwest" ,
"DESTINATION_REGION.Upper.Mountain"  ,  
"DELIVERY_LATE_DAYOFWEEK.Friday",
"DELIVERY_LATE_DAYOFWEEK.Monday" ,      
"DELIVERY_LATE_DAYOFWEEK.Saturday" ,
"DELIVERY_LATE_DAYOFWEEK.Sunday" ,      
"DELIVERY_LATE_DAYOFWEEK.Thursday" ,
"DELIVERY_LATE_DAYOFWEEK.Tuesday"   ,   
"DELIVERY_LATE_DAYOFWEEK.Wednesday",
"SRC_DST_MTCH.0"    ,                   
"SRC_DST_MTCH.1"  ,
"weight_bins.1"  ,                      
"weight_bins.2" ,
"weight_bins.3" ,                       
"weight_bins.4" , 
"ORDER_COST")

# Split the data into training and testin data sets
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
data1<-scaled
set.seed(1237482) 
library(caTools)
sample = sample.split(data1, SplitRatio = .70)
train = subset(data1, sample == TRUE)
test = subset(data1, sample == FALSE)

### we will divide the test into 2 parts to have a validation set
sample=sample.split(test,SplitRatio = .5)
test_data=subset(test,sample==TRUE)
eval_data=subset(test,sample==FALSE)

### drop the test dataset now and test1 will be out
n <- names(train)
f <- as.formula(paste("ORDER_COST ~", paste(n[!n %in% "ORDER_COST"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(5,3),rep=3,linear.output=T)
summary(nn)
nn$net.result
pr.nn <- compute(nn,train[,1:50])

# Accuracy testing
pr.nn_trn <- pr.nn$net.result*(max(data$ORDER_COST)-min(data$ORDER_COST))+min(data$ORDER_COST)
train.r <- (train$ORDER_COST)*(max(data$ORDER_COST)-min(data$ORDER_COST))+min(data$ORDER_COST)
train_op<-data.frame(train.r,pr.nn_trn)
result2 <- rmse(actual = train_op$train.r, predicted = train_op$pr.nn_trn)

pr.nn <- compute(nn,train[,1:50])
pr.nn_trn <- pr.nn$net.result*(max(data$ORDER_COST)-min(data$ORDER_COST))+min(data$ORDER_COST)
train.r <- (train$ORDER_COST)*(max(data$ORDER_COST)-min(data$ORDER_COST))+min(data$ORDER_COST)
train_op<-data.frame(train.r,pr.nn_trn)
result2 <- rmse(actual = train_op$train.r, predicted = train_op$pr.nn_trn)

pr.test <- compute(nn,test_data[,1:50])
pr_test_val <- pr.test$net.result*(max(data$ORDER_COST)-min(data$ORDER_COST))+min(data$ORDER_COST)
test.r <- (test_data$ORDER_COST)*(max(data$ORDER_COST)-min(data$ORDER_COST))+min(data$ORDER_COST)
test_op<-data.frame(test.r,pr_test_val)
result3 <- rmse(actual = test_op$test.r, predicted = test_op$pr_test_val)

pr.eval <- compute(nn,eval_data[,1:50])
pr_eval_val <- pr.eval$net.result*(max(data$ORDER_COST)-min(data$ORDER_COST))+min(data$ORDER_COST)
eval.r <- (eval_data$ORDER_COST)*(max(data$ORDER_COST)-min(data$ORDER_COST))+min(data$ORDER_COST)
eval_op<-data.frame(eval.r,pr_eval_val)
result4 <- rmse(actual = eval_op$eval.r, predicted = eval_op$pr_eval_val)
