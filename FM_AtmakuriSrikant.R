rm(list=ls(all=TRUE))
#rm(list=c('spotfreight1'))

#setwd("C:\\Srikant\\INSOFE\\Projects\\Freight Management\\Dataset_QuickFreight")
setwd("D:\\github_repo\\INSOFE_CPEE_Project_freightmgmt")

# Reads all Data
spotfreight <- read.csv("spotfreightdata.csv",header=T, na.strings = c("","NA"," "))
regionlookups <- read.csv("regionlookups.csv",header=T, na.strings = c("","NA"," "))
marketzips <- read.csv("marketzips.csv",header=T, na.strings = c("","NA"," "))
equipmentcodes <- read.csv("equipmentcodes.csv",header=T, na.strings = c("","NA"," "))

#View(spotfreight)
# View(regionlookups)
# View(marketzips)
# View(equipmentcodes)

# Understanding the data
str(spotfreight)
summary(spotfreight)

str(regionlookups)
summary(regionlookups)

str(marketzips)
summary(marketzips)

str(equipmentcodes)
summary(equipmentcodes)

# Preprocessing the data
sum(is.na(spotfreight)) # checks for any rows with NA in the spotfreight data
spotfreight <- na.omit(spotfreight) # removes all the rows with NA in one or more columns
summary(complete.cases(spotfreight)) # checks if any of the rows has NAs in any of its columns of the spotfreight data


cat("Total orders count : " , length(spotfreight$ORDER_NBR))  # 34693
cat("Unique orders count : " , length(unique(spotfreight$ORDER_NBR))) # 23512

# There is multiple rows for the same data

#"ORDER_NBR","EQUIPMENT_TYPE","CUSTOMER_MILES","WEIGHT","ORDER_COST","FIRST_PICK_ZIP","FIRST_PICK_EARLY_APPT","FIRST_PICK_LATE_APPT","LAST_DELIVERY_ZIP","LAST_DELIVERY_EARLY_APPT","LAST_DELIVERY_LATE_APPT","IS_HAZARDOUS","CREATED_DATE"
# 167555,"V",2509,12982,0,"47805","1/22/2016 0:00","1/22/2016 0:00","97321","1/25/2016 0:00","1/25/2016 0:00","N","1/5/2016 0:00"
# 167555,"V",2509,12982,0,"47805","1/22/2016 0:00","1/22/2016 0:00","97218","1/26/2016 0:00","1/26/2016 0:00","N","1/5/2016 0:00"
# 167555,"V",2509,12982,0,"47805","1/22/2016 0:00","1/22/2016 0:00","97218","1/25/2016 0:00","1/26/2016 0:00","N","1/5/2016 0:00"
# 167555,"V",2509,12982,0,"47805","1/22/2016 0:00","1/22/2016 0:00","98390","1/26/2016 0:00","1/26/2016 0:00","N","1/5/2016 0:00"
# 167555,"V",2509,12982,0,"47805","1/22/2016 0:00","1/22/2016 0:00","V3V4G1","1/26/2016 0:00","1/26/2016 0:00","N","1/5/2016 0:00"

# 169459,"V",716,9420,1500,"45011","1/21/2016 0:00","1/21/2016 0:00","47909","1/21/2016 0:00","1/22/2016 0:00","N","1/16/2016 0:00"
# 169459,"V",716,9420,1500,"47909","1/21/2016 0:00","1/22/2016 0:00","47909","1/21/2016 0:00","1/22/2016 0:00","N","1/16/2016 0:00"
# 169459,"V",716,9420,1500,"40330","1/21/2016 0:00","1/21/2016 0:00","47909","1/21/2016 0:00","1/22/2016 0:00","N","1/16/2016 0:00"
# 169459,"V",716,9420,1500,"45011","1/21/2016 0:00","1/21/2016 0:00","40330","1/22/2016 0:00","1/22/2016 0:00","N","1/16/2016 0:00"
# 169459,"V",716,9420,1500,"47909","1/21/2016 0:00","1/22/2016 0:00","40330","1/22/2016 0:00","1/22/2016 0:00","N","1/16/2016 0:00"
# 169459,"V",716,9420,1500,"40330","1/21/2016 0:00","1/21/2016 0:00","40330","1/22/2016 0:00","1/22/2016 0:00","N","1/16/2016 0:00"
# 169459,"V",716,9420,1500,"45011","1/21/2016 0:00","1/21/2016 0:00","45011","1/25/2016 0:00","1/25/2016 0:00","N","1/16/2016 0:00"
# 169459,"V",716,9420,1500,"47909","1/21/2016 0:00","1/22/2016 0:00","45011","1/25/2016 0:00","1/25/2016 0:00","N","1/16/2016 0:00"
# 169459,"V",716,9420,1500,"40330","1/21/2016 0:00","1/21/2016 0:00","45011","1/25/2016 0:00","1/25/2016 0:00","N","1/16/2016 0:00"

# remove duplicate records (ignoring the "CREATED_DATE" field)
spotfreight_unq <- spotfreight[!duplicated(spotfreight[,!(colnames(spotfreight) %in% c("CREATED_DATE"))]), ]

# remove records with alphanumeric zip codes either in FIRST_PICK_ZIP or LAST_DELIVERY_ZIP or both
spotfreight$FIRST_PICK_ZIP <- gsub("[ -]", "", spotfreight$FIRST_PICK_ZIP) # removes space and - characters from FIRST_PICK_ZIP
spotfreight$LAST_DELIVERY_ZIP <- gsub("[ -]", "", spotfreight$LAST_DELIVERY_ZIP) # removes space and - characters from LAST_DELIVERY_ZIP

check.numeric <- function(N){ !length(grep("[^[:digit:]]", as.character(N)))}

spotfreight$VALID_DELIVERY_ZIP <- sapply(spotfreight$LAST_DELIVERY_ZIP, check.numeric)
spotfreight$VALID_DELIVERY_ZIP <- sapply(spotfreight$LAST_DELIVERY_ZIP, check.numeric)

as.numeric(spotfreight$FIRST_PICK_ZIP)
which(is.na(as.numeric(as.character(spotfreight$FIRST_PICK_ZIP))))
sum(is.na(as.numeric(as.character(spotfreight$FIRST_PICK_ZIP))))
sapply(spotfreight$FIRST_PICK_ZIP, length(gsub("[:digit:]", "", unique(spotfreight$FIRST_PICK_ZIP))
sapply()
gsub("\\D+", "", "V2S7W6",ignore.case = TRUE)
gsub("[:digit:]", "", "67119",ignore.case = TRUE)
gsub("[ -]", "", " N5W 6C7")
L1W 3H9
N5W 6C7
spotfreight$FIRST_PICK_ZIP<-gsub("[\\S+-]", "", spotfreight$FIRST_PICK_ZIP)
!is.na(as.numeric(spotfreight$FIRST_PICK_ZIP))
check.numeric <- function(N){ !length(grep("[^[:digit:]]", as.character(N)))}
check.numeric("0")
# check & remove records with NA or zero values in ORDER_COST field (the target variable)

# check & remove records with zero or negative WEIGHT 

# 



library(plyr)
dfOrders <- as.data.frame(count(spotfreight$ORDER_NBR)) # creates a data frame with order number and corresponding frequency (repeation count)
#colnames(dfOrders)
dfOrders <- arrange(dfOrders, desc(freq)) # sorts the newly created data frame in descending order of the frequency column
# gives how many repeated orders exists with what repeatations in descending order of repeatations count
arrange(count(dfOrders, "freq"), desc(freq.1))
# freq freq.1
# 1     1  15482
# 2     2  13702
# 3     4   1880
# 4     6   1278
# 5     3    957
# 6     8    400
# 7     5    315
# 8    18    180
# 9     7    161
# 10   12    108
# 11   10    100
# 12    9     63
# 13   11     22
# 14   16     16
# 15   15     15
# 16   14     14


# One approach to remove NA or empty cells either assign NA at the time of reading the file and using na.omit() to remove all of NA
# Other option to use the below approach to remove after reading the file as-is
# removes both na and empty values in any columns of spotfreight. 
#spotfreight <- spotfreight[!apply(is.na(spotfreight) | spotfreight == "", 1, all),]
#summary(spotfreight)

#library(corrplot) # used for rendering the correlation chart
#spotfreight_cor <- cor(spotfreight[,3:5])
#spotfreight_cor
#corrplot(spotfreight_cor)

library(lubridate)
#zz <- as.data.frame(cbind(year = year(px), month = month(px), day = day(px)))
#sum(is.na(zz))

#px1 <- as.POSIXlt("1/13/2016 0:00", format="%m/%d/%Y %H:%M")
#zz1 <- as.data.frame(cbind(year = year(px1), month = month(px1), day = day(px1)))
#px1
#head(px1)
#head(spotfreight$FIRST_PICK_EARLY_APPT)

colnames(spotfreight)
# splitting the date&time cols to day,month,year cols
px1 <- as.POSIXlt(spotfreight$FIRST_PICK_EARLY_APPT, format="%m/%d/%Y %H:%M", tz="GMT")
px2 <- as.POSIXlt(spotfreight$FIRST_PICK_LATE_APPT, format="%m/%d/%Y %H:%M", tz="GMT")
px3 <- as.POSIXlt(spotfreight$LAST_DELIVERY_EARLY_APPT, format="%m/%d/%Y %H:%M", tz="GMT")
px4 <- as.POSIXlt(spotfreight$LAST_DELIVERY_LATE_APPT, format="%m/%d/%Y %H:%M", tz="GMT")
px5 <- as.POSIXlt(spotfreight$CREATED_DATE, format="%m/%d/%Y %H:%M", tz="GMT")

spotfreight1 <- spotfreight[, !(colnames(spotfreight) %in% c("FIRST_PICK_EARLY_APPT","FIRST_PICK_LATE_APPT","LAST_DELIVERY_EARLY_APPT", "LAST_DELIVERY_LATE_APPT", "CREATED_DATE"))]
colnames(spotfreight1)


spotfreight1 <- as.data.frame(cbind(spotfreight1, 
                                    PICK_EARLY_APPT_YEAR = year(px1), PICK_EARLY_APPT_MONTH = month(px1), PICK_EARLY_APPT_DAY = day(px1),
                                    PICK_EARLY_APPT_YEAR = year(px2), PICK_EARLY_APPT_MONTH = month(px2), PICK_EARLY_APPT_DAY = day(px2),
                                    PICK_EARLY_APPT_YEAR = year(px3), PICK_EARLY_APPT_MONTH = month(px3), PICK_EARLY_APPT_DAY = day(px3),
                                    PICK_EARLY_APPT_YEAR = year(px4), PICK_EARLY_APPT_MONTH = month(px4), PICK_EARLY_APPT_DAY = day(px4),
                                    PICK_EARLY_APPT_YEAR = year(px5), PICK_EARLY_APPT_MONTH = month(px5), PICK_EARLY_APPT_DAY = day(px5),
                                    ))