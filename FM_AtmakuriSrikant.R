rm(list=ls(all=TRUE))
#rm(list=c('spotfreight1'))

setwd("C:\\Srikant\\INSOFE\\Projects\\Freight Management\\Dataset_QuickFreight")

# Reads all Data
spotfreight <- read.csv("spotfreightdata.csv",header=T, na.strings = c("","NA"," "))
regionlookups <- read.csv("regionlookups.csv",header=T, na.strings = c("","NA"," "))
marketzips <- read.csv("marketzips.csv",header=T, na.strings = c("","NA"," "))
equipmentcodes <- read.csv("equipmentcodes.csv",header=T, na.strings = c("","NA"," "))

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
summary(spotfreight)

# One approach to remove NA or empty cells either assign NA at the time of reading the file and using na.omit() to remove all of NA
# Other option to use the below approach to remove after reading the file as-is
# removes both na and empty values in any columns of spotfreight. 
#spotfreight <- spotfreight[!apply(is.na(spotfreight) | spotfreight == "", 1, all),]
#summary(spotfreight)

library(corrplot) # used for rendering the correlation chart
colnames(spotfreight)
spotfreight_cor <- cor(spotfreight[,3:5])
spotfreight_cor
corrplot(spotfreight_cor)
