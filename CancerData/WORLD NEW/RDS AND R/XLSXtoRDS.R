#loading required libraries
library(xlsx)

#reading excel file and saving them in variables
MaleCancer <- read.xlsx("WorldCancerData.xlsx",1)
FemaleCancer <- read.xlsx("WorldCancerData.xlsx",2)
BothGenderCancer <- read.xlsx("WorldCancerData.xlsx",3)

#formatting Male Cancer Data and saving it as a data frame

cancerNames <- MaleCancer$Cancer
tempDF <- t(MaleCancer[ , -c(1)])
colnames(tempDF) <- cancerNames
MaleCancerFormatted <- as.data.frame(tempDF)

#formatting Female Cancer Data and saving it as a data frame

cancerNames <- FemaleCancer$Cancer
tempDF <- t(FemaleCancer[ , -c(1)])
colnames(tempDF) <- cancerNames
FemaleCancerFormatted <- as.data.frame(tempDF)

#formatting Both Gender Cancer Data and saving it as a data frame

cancerNames <- BothGenderCancer$Cancer
tempDF <- t(BothGenderCancer[ , -c(1)])
colnames(tempDF) <- cancerNames
BothGenderCancerFormatted <- as.data.frame(tempDF)

#renaming BothGenderCancerFormatted to BothSexesCancerFormatted
BothSexesCancerFormatted <- BothGenderCancerFormatted

#updating names of rows
name.of.rows <- c("0-14","15-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74")

row.names(MaleCancerFormatted) <- name.of.rows
row.names(FemaleCancerFormatted) <- name.of.rows
row.names(BothSexesCancerFormatted) <- name.of.rows

#saving variables in RDS files for later use
saveRDS(MaleCancerFormatted,"MaleCancerFormatted.rds")
saveRDS(FemaleCancerFormatted,"FemaleCancerFormatted.rds")
saveRDS(BothSexesCancerFormatted,"BothSexesCancerFormatted.rds")
