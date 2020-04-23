#Caden Zonnefeld | HW 1 | STAT 344

myData <- iris
myData$id <- 1:nrow(myData)
?iris
#-----------------------------------------------Question 1-----------------------------------------------------
sumRow <- function(x,i){
  numericColumns <- x[1:4]
  sum(numericColumns[i,])
}

sumRow(myData,2)
sumRow(myData,131)

#-----------------------------------------------Question 2-----------------------------------------------------
numericColumns <- myData[1:4]
rowSums <- vector(length=150L)
for (i in 1:150){
  rowSums[i] <- sum(numericColumns[i,])
}

#-----------------------------------------------Question 3-----------------------------------------------------
rowSums2 <- vector(length=150L)

sumRow2 <- function(x){
  numericColumns <- x[1:4]
  sum(numericColumns)
}
rowSums2 <- apply(myData[1:4],1,sumRow2)
rowSums2

#-----------------------------------------------Question 4-----------------------------------------------------
rowSums3 <- vector(length=150L)

#sumRow3 <- function(x){
#  numericColumns <- x[-5]
#  for(i in 1:nrow(x)){
#    rowSums3[i] <- sum(numericColumns[i,])
#  }
#  return(rowSums3)
#}

#sumRow3(myData)
#sapply(myData,sumRow3)


#could not figure out how to get sapply to iterate by row instead of column, the apply function had a
#a built in feature but the sapply does not as far as I can tell so I transposed the data such that
#I was able to use sapply
transposeData <- data.frame(t(myData[1:4]))

typeof(transposeData)

sumRow3 <- function(x){
  sum(x)
}
rowSums3 <- sapply(transposeData,sumRow3)
rowSums3

#-----------------------------------------------Question 5-----------------------------------------------------
install.packages('tidyverse')
library(tidyr)

longData <- gather(myData, key = "FlowerAttributes", value = "Values", Sepal.Length, Sepal.Width, Petal.Length,
                 Petal.Width)
longData

#-----------------------------------------------Question 6-----------------------------------------------------
aggregateNumeric <- aggregate(Values~FlowerAttributes,longData,mean)

aggregateNumeric["AverageValueForAttribute"] <- aggregateNumeric["Values"]
aggregateNumeric <- aggregateNumeric[-2]

aggregateNumeric

#-----------------------------------------------Question 7-----------------------------------------------------
longData <- merge(longData,aggregateNumeric,by="FlowerAttributes")
longData

#-----------------------------------------------Question 8-----------------------------------------------------
aggregateSpecies <- aggregate(Values~Species+FlowerAttributes,longData,mean)
aggregateSpecies["AverageValueForSpecies"] <- aggregateSpecies["Values"]
aggregateSpecies <- aggregateSpecies[-3]
aggregateSpecies


completeLongData <- merge(longData,aggregateSpecies,by=c("Species","FlowerAttributes"))
completeLongData
#-----------------------------------------------Question 9-----------------------------------------------------
#pivot_wider(completeLongData, names_from = FlowerAttributes, values_from = Values )
#pivot_wider(completeLongData, names_from = Values, values_from = FlowerAttributes)
#gather(completeLongData, key = "Species", value = c("FlowerAttributes", "Values", "Average Value for Species","Average Value for Attribute"))
#spread(completeLongData, key = "index",value="Average Value for Attribute","Average Value for Species")
#gather(completeLongData, key = "index",value="Values","FlowerAttributes")
#install.packages('reshape2')
#library(reshape2)
#dat <- dcast(completeLongData, formula=Species+AverageValueForSpecies+AverageValueForAttribute~FlowerAttributes,value.var="Values",fun.aggregate=length)

#compiled the data into a wide format, then dropped the id column
wideData <- dcast(completeLongData, formula=Species+AverageValueForSpecies+AverageValueForAttribute+id~FlowerAttributes,value.var="Values")
wideData <- wideData[-4]


#-----------------------------------------------Question 10----------------------------------------------------
#set directory, loaded file in, and set widths
setwd("Documents/STAT 344")
ia97 <- data.frame(read.fwf("est97-ia.dat.txt",widths=c(3,4,9,9,9,5,5,5,9,9,9,5,5,5,9,9,9,5,5,5,7,7,7,8,8,8,5,5,5,46,3)))
ia18 <- data.frame(read.fwf("est18-ia.dat.txt",widths=c(3,4,9,9,9,5,5,5,9,9,9,5,5,5,9,9,9,5,5,5,7,7,7,8,8,8,5,5,5,46,3,22)))

#added a year column for each dataset
ia97["Year"] <- 1997
ia18["Year"] <- 2018

library(dplyr)

#renamed columns for '97 and '18 data
ia97 <- ia97 %>%
  rename(StateCode = V1, CountyCode = V2, PovEstimate = V3, PovEstimate90CILower = V4, PovEstimate90CIUpper = V5,
         PovertyPerc = V6, PovertyPerc90CILower = V7, PovertyPerc90CIUpper = V8, PovEstimate0to17 = V9,
         PovEstimate0to1790CILower = V10, PovEstimate0to1790CIUpper = V11, PovEstimatePerc0to17 = V12,
         PovEstimatePerc0to1790CILower = V13, PovEstimatePerc0to1790CIUpper = V14, 
         EstimateChild5to17Pov = V15, EstimateChild5to17Pov90CILower = V16,
         EstimateChild5to17Pov90CIUpper = V17, EstimatedChild5to17PovPerc = V18,
         EstimatedChild5to17PovPerc90CILower = V19, EstimatedChild5to17PovPerc90CIUpper = V20,
         MedianHouseholdIncome = V21, MedianHouseholdIncome90CILower = V22, MedianHouseholdIncome90CIUpper = V23,
         EstimateChildUnder5Pov = V24, EstimateChildUnder5Pov90CILower = V25,
         EstimateChildUnder5Pov90CIUpper = V26, EstimatedChildUnder5PovPerc = V27,
         EstimatedChildUnder5PovPerc90CILower = V28, EstimatedChildUnder5PovPerc90CIUpper = V29,
         StateOrCountyName = V30, PostalStateCode = V31)
ia18 <- ia18 %>%
  rename(StateCode = V1, CountyCode = V2, PovEstimate = V3, PovEstimate90CILower = V4, PovEstimate90CIUpper = V5,
         PovertyPerc = V6, PovertyPerc90CILower = V7, PovertyPerc90CIUpper = V8, PovEstimate0to17 = V9,
         PovEstimate0to1790CILower = V10, PovEstimate0to1790CIUpper = V11, PovEstimatePerc0to17 = V12,
         PovEstimatePerc0to1790CILower = V13, PovEstimatePerc0to1790CIUpper = V14, 
         EstimateChild5to17Pov = V15, EstimateChild5to17Pov90CILower = V16,
         EstimateChild5to17Pov90CIUpper = V17, EstimatedChild5to17PovPerc = V18,
         EstimatedChild5to17PovPerc90CILower = V19, EstimatedChild5to17PovPerc90CIUpper = V20,
         MedianHouseholdIncome = V21, MedianHouseholdIncome90CILower = V22, MedianHouseholdIncome90CIUpper = V23,
         EstimateChildUnder5Pov = V24, EstimateChildUnder5Pov90CILower = V25,
         EstimateChildUnder5Pov90CIUpper = V26, EstimatedChildUnder5PovPerc = V27,
         EstimatedChildUnder5PovPerc90CILower = V28, EstimatedChildUnder5PovPerc90CIUpper = V29,
         StateOrCountyName = V30, PostalStateCode = V31, FileName = V32)

#added a margin of error column and doublechecked that the data agreed for each then dropped extra
ia97["MarginOfError"] <- abs(ia97$PovEstimate90CIUpper-ia97$PovEstimate)
#ia97["Margin of Error2"] <- abs(ia97$PovEstimate90CILower-ia97$PovEstimate)

ia18["MarginOfError"] <- abs(ia18$PovEstimate90CIUpper-ia18$PovEstimate)
#ia18["Margin of Error2"] <- abs(ia18$PovEstimate90CILower-ia18$PovEstimate)

#made a subset of the 1997 and 2018 data for the suggested variables of year, county, poverty estimate,
#margin of error for the poverty estimate, and poverty percentage estimate
#I also included other variables that could be of interest to this kind of an analyis such as:
#
ia97trimmed <- subset(ia97, select=c("Year","StateOrCountyName","PovEstimate","MarginOfError","PovertyPerc"))
ia18trimmed <- subset(ia18, select=c("Year","StateOrCountyName","PovEstimate","MarginOfError","PovertyPerc"))

#used an rbind to combine the data
cleanData <- rbind(ia97trimmed,ia18trimmed)


#renamed columns for clarity, added to years for each measure so that year columns could be dropped
cleanData <- cleanData %>%
  rename(County = StateOrCountyName, PovertyEstimate = PovEstimate, MarginOfError = MarginOfError, 
         PovertyPercentageEstimate = PovertyPerc)


#left entire state of Iowa as row 1 and 101, unsure of whether or not you wanted this removed
cleanData

#write the data into a comma seperated value file
write.csv(cleanData,file="cleanSAIPEdata.csv")
