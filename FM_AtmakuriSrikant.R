rm(list=ls(all=TRUE))
#rm(list=c('spotfreight1'))

#setwd("C:\\Srikant\\INSOFE\\Projects\\Freight Management\\Dataset_QuickFreight")
setwd("D:\\github_repo\\INSOFE_CPEE_Project_freightmgmt")

# Reads all Data
spotfreight <- read.csv("spotfreightdata.csv",header=T, na.strings = c("","NA"," "))    # 37699 obs
regionlookups <- read.csv("regionlookups.csv",header=T, na.strings = c("","NA"," "))    # 906 obs
marketzips <- read.csv("marketzips.csv",header=T, na.strings = c("","NA"," "))          # 905 obs
equipmentcodes <- read.csv("equipmentcodes.csv",header=T, na.strings = c("","NA"," "))  # 292 obs

library(dplyr)
library(lubridate)
library(plyr)

# View(spotfreight)
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
sum(is.na(spotfreight)) # checks for any rows with NA in the spotfreight data = 5620 na's 
spotfreight <- na.omit(spotfreight) # removes all the rows with NA in one or more columns =  34693 records 
summary(complete.cases(spotfreight)) # checks if any of the rows has NAs in any of its columns of the spotfreight data


cat("Total orders count : " , length(spotfreight$ORDER_NBR))  # 34693
cat("Unique orders count : " , n_distinct(spotfreight$ORDER_NBR)) # 23512

# There are multiple rows for the same data (like order_number with slight difference in zip, appt, etc...). Below are such samples:
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

# removes space and - characters from both FIRST_PICK_ZIP & LAST_DELIVERY_ZIP columns
spotfreight$FIRST_PICK_ZIP <- gsub("[ -]", "", spotfreight$FIRST_PICK_ZIP) 
spotfreight$LAST_DELIVERY_ZIP <- gsub("[ -]", "", spotfreight$LAST_DELIVERY_ZIP)

# remove records with alphanumeric zip codes either in FIRST_PICK_ZIP or LAST_DELIVERY_ZIP or both
check.numeric <- function(N){ !length(grep("[^[:digit:]]", as.character(N)))}
spotfreight <- spotfreight[sapply(spotfreight$FIRST_PICK_ZIP, check.numeric) & sapply(spotfreight$LAST_DELIVERY_ZIP, check.numeric),] 
#spotfreight$VALID_PICK_ZIP <- sapply(spotfreight$FIRST_PICK_ZIP, check.numeric)
#spotfreight$VALID_PICK_ZIP <- NULL
#spotfreight$VALID_DELIVERY_ZIP <- sapply(spotfreight$LAST_DELIVERY_ZIP, check.numeric)
#spotfreight$VALID_DELIVERY_ZIP <- NULL

# check & remove records with zero values in ORDER_COST field (the target variable)
summary(spotfreight$ORDER_COST)
arrange(summarise(group_by(spotfreight, ORDER_COST), orders = n()), desc(-ORDER_COST)) # there are around 6326 records with zero ORDER_COST (the target variable)
n_distinct(spotfreight) # 34273
spotfreight <- spotfreight[!((spotfreight$ORDER_COST == 0) | (spotfreight$ORDER_COST == 0.01)),] 
n_distinct(spotfreight) # 27946 (6327 records with  0 ORDER_COST excluded )

# check & remove records with zero or negative WEIGHT
summary(spotfreight$WEIGHT)
arrange(summarise(group_by(spotfreight, WEIGHT), orders = n()), desc(-WEIGHT)) # there are around 1681 records with zero WEIGHT (to be excluded)
spotfreight <- spotfreight[!(spotfreight$WEIGHT == 0),] 
n_distinct(spotfreight) # 26265 (1681 records with 0 WEIGHT excluded )

# check & remove records with zero or negative CUSTOMER_MILES
summary(spotfreight$CUSTOMER_MILES)
arrange(summarise(group_by(spotfreight, CUSTOMER_MILES), orders = n()), desc(-CUSTOMER_MILES)) # there are around 1681 records with zero CUSTOMER_MILES (to be excluded)
spotfreight <- spotfreight[!(spotfreight$CUSTOMER_MILES == 0),] 
n_distinct(spotfreight) # 26166 (99 records with 0 CUSTOMER_MILES excluded )

# check & understand equipments types and add 2 new columns based on the values in equipmentcodes dataframe and equipment_type value in spotfreight data frame
summary(spotfreight$EQUIPMENT_TYPE) # this is to understand how many different types of equipments are avaialable in the given raw  data
equipments <- arrange(summarise(group_by(spotfreight, EQUIPMENT_TYPE), orders = n()), desc(orders)) # this is to understand how orders with what equipment type code in the raw data
spotfreight$Equipment_New_Abbr <- equipmentcodes[match(spotfreight$EQUIPMENT_TYPE, equipmentcodes$EQUIPMENT_TYPE),2]
names(spotfreight)[names(spotfreight) == 'Equipment_New_Abbr'] <- 'EQUIPMENT_NEW_ABBR' # renames a column name from old to a new name
summary(spotfreight$EQUIPMENT_NEW_ABBR)
spotfreight$EQUIPMENT_TYPE_DESC <- equipmentcodes[match(spotfreight$EQUIPMENT_TYPE, equipmentcodes$EQUIPMENT_TYPE),3]
summary(spotfreight$EQUIPMENT_TYPE_DESC)

spotfreight_Missing_EQUIPMENT_TYPE <- spotfreight[is.na(spotfreight$EQUIPMENT_NEW_ABBR) | is.na(spotfreight$EQUIPMENT_TYPE_DESC),]
spotfreight <- na.omit(spotfreight) # 95records removed for which EQUIPMENT_ABBR / EQUIPMENT_TYPE_DESC isn't found from equipmentcodes dataframe based on the eequipment type code in spotfreight data frame
summary(complete.cases(spotfreight)) # 26071 obs.

# check & remove records with zero or negative CUSTOMER_MILES
summary(spotfreight$CUSTOMER_MILES)
arrange(summarise(group_by(spotfreight, CUSTOMER_MILES), orders = n()), desc(-CUSTOMER_MILES)) # there are around 1681 records with zero CUSTOMER_MILES (to be excluded)
spotfreight <- spotfreight[!(spotfreight$CUSTOMER_MILES == 0),] 
n_distinct(spotfreight) # 26166 (99 records with 0 CUSTOMER_MILES excluded )


#which(is.na(as.numeric(as.character(spotfreight$FIRST_PICK_ZIP))))
#sum(is.na(as.numeric(as.character(spotfreight$FIRST_PICK_ZIP))))
#sapply(spotfreight$FIRST_PICK_ZIP, length(gsub("[:digit:]", "", unique(spotfreight$FIRST_PICK_ZIP))
#gsub("\\D+", "", "V2S7W6",ignore.case = TRUE)
#gsub("[:digit:]", "", "67119",ignore.case = TRUE)
#gsub("[ -]", "", " N5W 6C7")
#spotfreight$FIRST_PICK_ZIP<-gsub("[\\S+-]", "", spotfreight$FIRST_PICK_ZIP)
#!is.na(as.numeric(spotfreight$FIRST_PICK_ZIP))
#check.numeric <- function(N){ !length(grep("[^[:digit:]]", as.character(N)))}
#check.numeric("0")


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


#zz <- as.data.frame(cbind(year = year(px), month = month(px), day = day(px)))
#sum(is.na(zz))

#px1 <- as.POSIXlt("1/13/2016 0:00", format="%m/%d/%Y %H:%M")
#zz1 <- as.data.frame(cbind(year = year(px1), month = month(px1), day = day(px1)))
#px1
#head(px1)
#head(spotfreight$FIRST_PICK_EARLY_APPT)


# splitting the date&time cols to day,month,year cols
# reference: http://www.cyclismo.org/tutorial/R/time.html
# 2 different types specific to time data fields in R. the POSIXct and POSIXlt 
# POSIXct : this data type is the number of seconds since the start of January 1, 1970. Negative numbers represent the number of seconds before this time, and positive numbers represent the number of seconds afterwards
# POSIXlt : this data type is a vector, and the entries in the vector have different meanings for day, month, year, week, etc... of that date
# strftime : this function is used to take a time data type and convert it to a string. 

spotfreight$FIRST_PICK_EARLY_APPT <- as.POSIXlt(spotfreight$FIRST_PICK_EARLY_APPT, format="%m/%d/%Y %H:%M", tz="GMT")
spotfreight$FIRST_PICK_LATE_APPT <- as.POSIXlt(spotfreight$FIRST_PICK_LATE_APPT, format="%m/%d/%Y %H:%M", tz="GMT")
spotfreight$LAST_DELIVERY_EARLY_APPT <- as.POSIXlt(spotfreight$LAST_DELIVERY_EARLY_APPT, format="%m/%d/%Y %H:%M", tz="GMT")
spotfreight$LAST_DELIVERY_LATE_APPT <- as.POSIXlt(spotfreight$LAST_DELIVERY_LATE_APPT, format="%m/%d/%Y %H:%M", tz="GMT")
spotfreight$CREATED_DATE <- as.POSIXlt(spotfreight$CREATED_DATE, format="%m/%d/%Y %H:%M", tz="GMT")

spotfreight1 <- spotfreight[, !(colnames(spotfreight) %in% c("FIRST_PICK_EARLY_APPT","FIRST_PICK_LATE_APPT","LAST_DELIVERY_EARLY_APPT", "LAST_DELIVERY_LATE_APPT", "CREATED_DATE"))]
colnames(spotfreight1)


spotfreight1 <- as.data.frame(cbind(spotfreight1, 
                                    PICK_EARLY_APPT_YEAR = year(px1), PICK_EARLY_APPT_MONTH = month(px1), PICK_EARLY_APPT_DAY = day(px1),
                                    PICK_EARLY_APPT_YEAR = year(px2), PICK_EARLY_APPT_MONTH = month(px2), PICK_EARLY_APPT_DAY = day(px2),
                                    PICK_EARLY_APPT_YEAR = year(px3), PICK_EARLY_APPT_MONTH = month(px3), PICK_EARLY_APPT_DAY = day(px3),
                                    PICK_EARLY_APPT_YEAR = year(px4), PICK_EARLY_APPT_MONTH = month(px4), PICK_EARLY_APPT_DAY = day(px4),
                                    PICK_EARLY_APPT_YEAR = year(px5), PICK_EARLY_APPT_MONTH = month(px5), PICK_EARLY_APPT_DAY = day(px5)
                                    ))