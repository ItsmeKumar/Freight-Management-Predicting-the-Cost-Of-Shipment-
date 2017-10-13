###
### Data Pre Processing 
### The end product from here is used for applying models to predict shipment cost
###

setwd("~/Downloads/project")

# Read the data
data1<-read.csv("spotfreightdata.csv",sep=",",header=T)
data1[data1==""] <- NA

# Remove null values
sum(is.na(data2))
data2<-na.omit(data1)

# Remove unwanted values
data3<-subset(data2,WEIGHT != 0.0,all=T)
data4<-subset(data3,CUSTOMER_MILES !=0.0,all=T)
data5<-subset(data4,ORDER_COST != 0.0,all=T)
data6<-subset(data5,ORDER_COST == 0.0,all=T)

# Save file
write.table(data,"project_fnl_data1.csv",append=TRUE,row.names=FALSE,sep=",",quote = FALSE)

# Read saved file, to further pre process
data_raw<-read.csv("project_fnl_data.csv",sep=",",header=T)
# convert required columns to characters
c<-as.character(data_raw[,7])
e<-as.character(data_raw[26182,11])
# convert required columns to dates
date1<-as.Date(data_raw[26182,7], "%m/%d/%Y %H:%M:%S")
date2<-as.Date(data_raw[26182,11], "%m/%d/%Y %H:%M")

# convert required columns to posixct
date1<-as.POSIXct(strptime(data_raw[26179,7], "%m/%d/%Y %H:%M"))
date2<-as.POSIXct(strptime(data_raw[26179,11], "%m/%d/%Y %H:%M"))
as.numeric(difftime(date2, date1, units="mins"))

# Check for nulls, and convert required columns
data1<-data_raw[1:100,]
i=1
c<-as.character(data1[3,11])
date1<-as.POSIXct(strptime(data1[3,7], "%m/%d/%Y %H:%M"))
date2<-as.POSIXct(strptime(data1[3,11], "%m/%d/%Y %H:%M"))
z<-as.numeric(difftime(date2, date1, units="mins"))
is.na(z)
if(is.na(z))
{
  date2<-as.POSIXct(strptime(data1[3,10], "%m/%d/%Y %H:%M"))
  z<-as.numeric(difftime(date2, date1, units="mins")) 
}

# Process all the rows, apply POSIXct conversion
data1<-data_raw
t<-0
for(i in 1:nrow(data1))
{
  date1<-as.POSIXct(strptime(data1[i,7], "%m/%d/%Y %H:%M"))
  date2<-as.POSIXct(strptime(data1[i,11], "%m/%d/%Y %H:%M"))
  t[i]<-as.numeric(difftime(date2, date1, units="mins"))
  if(is.na(t[i]))
  {
    date2<-as.POSIXct(strptime(data1[i,10], "%m/%d/%Y %H:%M"))
    t[i]<-as.numeric(difftime(date2, date1, units="mins")) 
  }
}

# Take subset of data wherver required
data1$time<-t
sum(is.na(data1))
data2<-subset(data1,((time/60)/24) > 60 ,all=T)
data3<-subset(data1,((time/60)/24) < 60 ,all=T)
data4<-subset(data1,time!=0,all=T)

### There are 1063 for which the time difference between first pick up date and time 
## and late delivery date and time
t<-0
for(i in 1:nrow(data4))
{
  
  date1<-as.POSIXct(strptime(data4[i,10], "%m/%d/%Y %H:%M"))
  date2<-as.POSIXct(strptime(data4[i,11], "%m/%d/%Y %H:%M"))
  t[i]<-as.numeric(difftime(date2, date1, units="mins"))
}

# Plot the data to see the weight vs order cost relation
data4$time2<-t
rm(t)
data4<-(subset(data3,time > 0,all=T))
str(data3)
options(scipen=9)
plot(data4$WEIGHT,data4$ORDER_COST)
summary(data$time)
rm(data4)
summary(data4)

# Merge the data with equipment codes data 
unq_eqp<-unique(data4$EQUIPMENT_TYPE)
unq_eqp<-as.character(unq_eqp)
length(unq_eqp)
unq_eqp[1]
equip<-read.csv("equipmentcodes.csv",sep=",",header=T)
equip[1,1]==unq_eqp[2]
rm(data5)
data5<-data4[1:100,]
nrow(data5)
str(data5)
data4$EQUIPMENT_TYPE<-as.character(data4$EQUIPMENT_TYPE)

# Plot is again, for different equipment types
library(plyr)
data4$EQUIPMENT_TYPE<-revalue(data4$EQUIPMENT_TYPE, 
                      c("R"="REFRIGERATED",
                        "V"="DRY FREIGHT",
                        "VM"="DRY FREIGHT",
                        "VR"="DRY FREIGHT",
                        "LTL"="DRY FREIGHT",
                        "IMDL"="DRY FREIGHT",
                        "F"="FLATBED",
                        "VZ"="DRY FREIGHT",
                        "R-LTL"="REFRIGERATED",
                        "SD"="FLATBED",
                        "RM"="REFRIGERATED",
                        "RZ"="REFRIGERATED",
                        "HB"="DRY FREIGHT",
                        "SB"="DRY FREIGHT",
                        "DRAY"="DRY FREIGHT",
                        "HS"="DRY FREIGHT",
                        "VG"="DRY FREIGHT",
                        "D-LTL"="DRY FREIGHT",
                        "VS"="DRY FREIGHT",
                        "PO"="DRY FREIGHT",
                        "VCAR"="DRY FREIGHT",
                        "FT-4"="FLATBED",
                        "LB"="FLATBED",
                        "VA"="DRY FREIGHT",
                        "VDROP"="DRY FREIGHT",
                        "VV"="DRY FREIGHT"))
str(data4$EQUIPMENT_TYPE)
data4$EQUIPMENT_TYPE<-as.factor(data4$EQUIPMENT_TYPE)
plot(data4$ORDER_COST~data4$EQUIPMENT_TYPE)
mktzip<-read.csv("regionlookups.csv",sep=",",header=T)

# Remove the characters, and subset based on market data columns
str(x1$FIRST_PICK_ZIP)
x1<-subset(data4,substr(as.character(data4$FIRST_PICK_ZIP),1,1)=="0" | substr(as.character(data4$LAST_DELIVERY_ZIP),1,1)=="0"
           ,all=T)
nrow(mktzip)
for (i in 1:nrow(mktzip))
{
  if(nchar(mktzip[i,1]) == 1)
  {
    z[i]<-paste0("00",mktzip[i,1])
  }
  else
     if(nchar(mktzip[i,1]) == 2)
  {
    z[i]<-paste0("0",mktzip[i,1])
     }
  else
  {
    z[i]=mktzip[i,1]
  }
}

# Pattern match the data, and remove the unwanted records
mktzip$new<-z
c1<-substr(as.character(x1$FIRST_PICK_ZIP),1,3)
str(mktzip$new)
x2<-mktzip[match(c1,mktzip[,3]),2]
x1$first_pck_rgn<-x2
colnames(x1)
x1<-x1[,c(1,2,3,4,5,6,15,7,8,9,10,11,12,13,14)]

# Substring the data
c2<-substr(as.character(x1$LAST_DELIVERY_ZIP),1,3)
x3<-mktzip[match(c2,mktzip[,3]),2]
x1$last_del_rgn<-x3
x1<-x1[,c(1,2,3,4,5,6,7,8,9,10,16,11,12,13,14,15)]

# Take the matches and plot it to check for sanity
src_reg<-substr(as.character(data4$FIRST_PICK_ZIP),1,3)
src_regn<-mktzip[match(src_reg,mktzip[,3]),2]
data4$SOURCE_REGION <-src_regn
dest_reg<-substr(as.character(data4$LAST_DELIVERY_ZIP),1,3)
dest_regn<-mktzip[match(dest_reg,mktzip[,3]),2]
data4$DESTINATION_REGION <-dest_regn
data4<-data4[,c(1,2,3,4,5,6,15,7,8,9,16,10,11,12,13,14)]
sum(is.na(data4))
data4<-na.omit(data4)
ggplot(data4, aes(x=data4$WEIGHT, y=data4$ORDER_COST)) + 
  geom_bar(stat="identity")

# Discretize the data
d <- discretize(data4$WEIGHT, disc="equalwidth")
data4$weight_bins<-d$X
plot(data4$ORDER_COST~data4$weight_bins)
str(data4)
dayof<-as.character(data4$LAST_DELIVERY_LATE_APPT)
dayofwk<-weekdays(as.Date(dayof,"%m/%d/%Y"))
data4$DELIVERY_LATE_DAYOFWEEK<-dayofwk

# Check the covariance and other stats to remove VIF stats
str(data4)
data4$DELIVERY_LATE_DAYOFWEEK<-as.factor(data4$DELIVERY_LATE_DAYOFWEEK)
plot(data4$ORDER_COST~data4$DELIVERY_LATE_DAYOFWEEK)
str(data4$SOURCE_REGION)
str(data4$DESTINATION_REGION)
plot(data4$ORDER_COST~data4$DESTINATION_REGION)
cov(data4$ORDER_COST,data4$time)
summary(data4$ORDER_COST)
range(data4$WEIGHT)
range(data4$WEIGHT)[1]:range(data4$WEIGHT)[2]
d<-pretty(data4$WEIGHT,30)

# Plot the data to remove unrequired data
library(vegan)
datafst<-data.frame(data4$WEIGHT,data4$ORDER_COST)
mydat2 <- decostand(datafst,"range") 
fit<-kmeans(mydat2,centers=15) 
fit$centers
sum(fit$withinss)
fit$cluster
lstreg<-ifelse(data4$SOURCE_REGION==data4$DESTINATION_REGION,1,0)
str(lstreg)
lstreg[1:10]
data4$SRC_DST_MTCH<-lstreg
str(data4$SRC_DST_MTCH)
data4$SRC_DST_MTCH<-as.factor(data4$SRC_DST_MTCH)
str(data4)
plot(data4$ORDER_COST~data4$SRC_DST_MTCH)
