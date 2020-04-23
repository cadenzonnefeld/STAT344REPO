#Caden Zonnefeld | HW 2 | STAT 344

#-----------------------------------------------Question 1-----------------------------------------------------
setwd("Documents/STAT 344")
messyData <- data.frame(read.csv("messy data", header=TRUE, stringsAsFactors = FALSE,na.strings=c("","NA")))

#Capitalization convention varies in the first and last columns
#Question 3 answers have inconsistent format despite being a Yes or No question
#Question 2 was only answered once (difficult to draw conclusions)
#Harrey wedel entered his entire name into the first box
#There is a double entry of Haley Brown information (the first is incomplete)
#Henry Kunin's weight appears to be incorrect as it is nearly 4 times the next largest weight value
#Solomon did not enter a last name
#There are missing values for answers to Question 1 and 3
#Some column names uncapitalized

#-----------------------------------------------Question 2-----------------------------------------------------

#Harry Wedel appears to be listed in both the first and last name column
#Entered in as "Harry wedel" in the first name and "Wedel" in the last name column


#-----------------------------------------------Question 3-----------------------------------------------------
extraComments <- read.csv("extra comments.csv",header=TRUE,stringsAsFactors = FALSE)

matches <- match(extraComments$Last,messyData$Last)

for(i in 1:length(extraComments$comments)){
  if (is.na(matches[i])==TRUE){
    matches <- matches[-i]
    extraComments <- extraComments[-i,]
  }
}


for(i in 1:length(extraComments$comments)){
  if (is.na(messyData[matches[i],8])==TRUE){
    messyData[matches[i],8] <- extraComments$comments[i]
  }
}

 #-----------------------------------------------Question 4-----------------------------------------------------

messyData$Q3 <- gsub("n","N",messyData$Q3)
messyData$Q3 <- gsub("[^N]+","Y",messyData$Q3)

#-----------------------------------------------Question 5-----------------------------------------------------

messyData$Last <- strsplit(as.character(messyData$Last),split="-\\w+")

#-----------------------------------------------Question 6-----------------------------------------------------

messyData$weightWhole <- strsplit(as.character(messyData$weight),split="\\.\\d")
messyData$weightDecimal <- gsub("\\d{2}\\d?\\.","",as.character.numeric_version(messyData$weight))

#-----------------------------------------------Question 7-----------------------------------------------------

#dropping Harry Wedel's last name from the first name box
messyData$First <- gsub("\\s\\w+","",as.character(messyData$First))

#dropping the double entry of Haley Brown and reindexing the rows
messyData <- messyData[-4,]
row.names(messyData) <- 1:37

#rename(capitalize) and reorder columns
messyData["Age"] <- messyData["age"]
messyData["Weight"] <- messyData["weight"]
messyData["Comments"] <- messyData["comments"]
messyData["Weight Whole Number"] <- messyData["weightWhole"]
messyData["Weight Decimal"] <- messyData["weightDecimal"]
messyData <- messyData[-c(3,4,8,9,10)]

#I chose to leave out question 2 as it was only answered one time and this does not help in drawing conclusions
messyData <- messyData[c("First","Last","Age","Weight","Weight Whole Number", "Weight Decimal","Q1","Q3","Comments")]

#I chose to leave Harry Kunin's weight as is though it appears to be a misentry
#Regulating the capitlization of first and last names
messyData$First <- paste0(toupper(substr(messyData$First,1,1)),tolower(substr(messyData$First,2,8)))
messyData$Last <- paste0(toupper(substr(messyData$Last,1,1)),tolower(substr(messyData$Last,2,10)))
messyData[16,2] <- NA

#cannot do anything about missing data values or missing last name for Solomon

#and lastly of course :)

cleanData <- messyData


#-----------------------------------------------Question 8-----------------------------------------------------

setwd("Documents/STAT 344")

#downloading layout files
layouts <- list()
layoutDirectory <- dir(pattern="\\d+-estimate-layout.txt")

for (i in 1:length(layoutDirectory)){
  layouts[[i]] <- readLines(layoutDirectory[i])
}

#downloading estimate data files
#dataFiles <- list()
dataDirectory <- dir(pattern="\\d{4}ia.txt")

#took a different approach that no longer needed this
#for (i in 1:length(dataDirectory)){
#  dataFiles[[i]] <- readLines(dataDirectory[i])
#}

#scraping indices for the county name locations
countyStartIndex <- list()
countyEndIndex <- list()
county <- list()

for(i in 1:length(layouts)){
  county[i] <- grep("[0-9]",layouts[[i]][grep("State or county name",layouts[[i]])],value=TRUE)
  county[i] <- gsub("[^0-9-]","",county[i])
  
  countyStartIndex[i] <- as.numeric(gsub("-\\d\\d?\\d?","",county[i]))
  countyEndIndex[i] <- as.numeric(gsub("\\d\\d?\\d?-","",county[i]))
}

countyStartIndices <- unlist(countyStartIndex)
countyEndIndices <- unlist(countyEndIndex)


#scraping indices for poverty percentage estimate indices
povEstimateStartIndex <- list()
povEstimateEndIndex <- list()
povEstimate <- list()

for (i in 1:length(layouts)){
  povEstimate[i] <- grep("[0-9]",layouts[[i]][grep("Estimated percent of people of all ages in poverty",
                                                   layouts[[i]])],value=TRUE)
  povEstimate[i] <- gsub("[^0-9-]","",povEstimate[i])
  
  povEstimateStartIndex[i] <- as.numeric(gsub("-\\d\\d?\\d?","",povEstimate[i]))
  povEstimateEndIndex[i] <- as.numeric(gsub("\\d\\d?\\d?-","",povEstimate[i]))
}

povEstimateStartIndices <- unlist(povEstimateStartIndex)
povEstimateEndIndices <- unlist(povEstimateEndIndex)


#scraping indices for the upper bound of the confidence interval of estimated poverty percentage
#so that the margin of error can be calculated later
upperBoundStartIndex <- list()
upperBoundEndIndex <- list()
upperBound <- list()

for (i in 1:length(layouts)){
  upperBoundMatches <- grep("90% confidence interval upper bound of",layouts[[i]])
  upperBound[i] <- grep("[0-9]",layouts[[i]][upperBoundMatches[2]],value=TRUE)
  upperBound[i] <- gsub("[^0-9-]","",upperBound[i])
  upperBound[i] <- gsub("90","",upperBound[i])
  
  upperBoundStartIndex[i] <- as.numeric(gsub("-\\d\\d?\\d?","",upperBound[i]))
  upperBoundEndIndex[i] <- as.numeric(gsub("\\d\\d?\\d?-","",upperBound[i]))
}

upperBoundStartIndices <- unlist(upperBoundStartIndex)
upperBoundEndIndices <- unlist(upperBoundEndIndex)



#list that contains the inddices for the three variables of interest
indexVector <- list()

for (i in 1:length(layouts)){
  indexVector[[i]] <- c(povEstimateStartIndices[i],povEstimateEndIndices[i],upperBoundStartIndices[i],
                      upperBoundEndIndices[i],countyStartIndices[i],countyEndIndices[i])
}

#constructing a widths list
widths <- list()

for (i in 1:length(layouts)){
  #same since the indices do not change across the years
  #the first 34 characters before poverty percentage estimate which we do not care about
  #the next 4 characters capture the poverty percentage estimate
  #the next 6 characters are the gap up until the upper bound of the poverty percentage estimate 90% CI
  #the next 4 characters capture the poverty percentage estimate upper bound of the 90% CI
  #the next 145 characters are the gap before county
  #the last 45 characters capture county name
  widths[[i]] <- c(34,4,6,4,145,45)
}

#reading in files and trimming data for the essentials
trimmedData <- list()

#began by reading the lines in from each file in the data directory and using the values from the widths vector i created earlier
#kept the 2nd, 4th, and 6th vectors as they held the information i desired
#renamed the county and percentage of people in poverty estimate
#calculated the margin of error on the percentage estimate
#assigned the year based off of the file name to each set of the three existing vectors
#deleted the unnamed vector versions from the raw data
for (i in 1:length(layouts)){
  trimmedData[[i]] <- read.fwf(dataDirectory[i],widths[[i]])
  trimmedData[[i]] <- trimmedData[[i]][c(2,4,6)]
  
  trimmedData[[i]]["County"] <- trimmedData[[i]][3]
  trimmedData[[i]]["Percentage of People in Poverty Estimate"] <- trimmedData[[i]][1]
  trimmedData[[i]]["Margin of Error for Estimate"] <- trimmedData[[i]][2]-trimmedData[[i]][1]
  trimmedData[[i]]["Year"] <- substr(dataDirectory[i],1,4)
  trimmedData[[i]] <- trimmedData[[i]][c(4,5,6,7)]
}

#my finalData is more inclined to be read as a list of data frames however i converted it into a dataframe and it takes on a wide format
#the data is all correct, but the column names take on a different form, i decided not to change that as i thought your main goal was for
#a list of datasets, let me know if you want me to change the column names
finalData <- trimmedData

finalData

finalDataDF <- data.frame(trimmedData)
