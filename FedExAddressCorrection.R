##FedEx Address Correction Detail Report Analysis
##Created by Michael Green 
##November 2015

##Run report on FedEx.com for desired time frame and download to desktop in csv format
##load report into R as follows:

library("dplyr")
library("plyr")
library("sqldf")
library("xlsx")
data(zipcode)
setwd("~/Desktop") 
##must enter correct name of report below:
addcorr <- read.csv("FedEx_Address_Correction_Detail_Payer_Detail_000000.csv", header = TRUE)

##to make slicing easier we want to control for a few things:
addcorr$Recipient.Name <- as.character(addcorr$Recipient.Name)
addcorr$Recipient.Name <- tolower(addcorr$Recipient.Name)
addcorr$Recipient.Address <- as.character(addcorr$Recipient.Address)
addcorr$Recipient.Address <- tolower(addcorr$Recipient.Address)
addcorr$Recipient.Address.Line2 <- as.character(addcorr$Recipient.Address.Line2)
addcorr$Recipient.Address.Line2 <- tolower(addcorr$Recipient.Address.Line2)
addcorr$Recipient.Original.Address <- as.character(addcorr$Recipient.Original.Address)
addcorr$Recipient.Original.Address <- tolower(addcorr$Recipient.Original.Address)
addcorr$Recipient.Original.Address.Line2 <- as.character(addcorr$Recipient.Original.Address.Line2)
addcorr$Recipient.Original.Address.Line2 <- tolower(addcorr$Recipient.Original.Address.Line2)
addcorr <- mutate(addcorr, count = 1)


##data is ready for analysis, below are a couple ways to cut it that I have found useful
count_domintl <- ddply(addcorr, c("Dom_Intl"), summarise, tot = sum(count))
countstate <- ddply(addcorr, c("Recipient.State"), summarise, tot = sum(count))
colnames(count_domintl) <- c("Recipient.State", "tot")
geog <- rbind(count_domintl, countstate)
write.xlsx(geog, "FedEx_Address_Correction_Analysis.xlsx", sheetName='Geog_Summary')

##Examining these slices will most likely give you numbers that reflect your general shipment profile. 
##i.e. higher total volumes = higher address corrections

##Most useful for lowering costs and minimizing issues is to find any repeat offenders
##CS can then reach out to these customers to 'address' the issue

#count the number of times each recipient shows up in the report
offenders <- ddply(addcorr, c("Recipient.Name"), summarise, tot = sum(count))
offenders <- arrange(offenders, desc(tot))
## how much could it save you?
totaloffenses <- sum(as.numeric(offenders$tot))
savings <- totaloffenses*12.50
report <- c("Total expense", savings)
offenders <- rbind(offenders, report)
write.xlsx(offenders, "FedEx_Address_Correction_Analysis.xlsx", sheetName='Offenders', append=TRUE)

##drill down -- are people requiring corrections to the SAME address or are they shipping to new addresses with the same one on file?

##This can be analyzed by comparing each address variable entered by customer - Recipient.Address, Recipient.Postal.Code, Recipient.City, Recipient.State
##to what FedEx charged to correct it to (add 'Original.' after recipient to the variable of concern i.e. 'Recipient.Original.Address')
offenders <- ddply(addcorr, c("Recipient.Name", "Recipient.Address", "Recipient.Original.Address", "Recipient.Postal.Code", "Recipient.Original.Postal.Code","Recipient.State","Recipient.Original.State", "Recipient.Original.City", "Recipient.City", "Recipient.Address.Line2", "Recipient.Original.Address.Line2"), summarise, tot = sum(count))
multipleoffenders <- filter(offenders, tot > 1)
multipleoffenders <- arrange(multipleoffenders, desc(tot))


## to quickly analyze if Address is different
## to quickly analyze if PostalCode (US only) is different - must make fedex postal code 5 digit from potential 9 digit and pass indentical
multipleoffenders$Recipient.Postal.Code <- as.numeric(as.character(substr(multipleoffenders$Recipient.Postal.Code, 1, 5)))
multipleoffenders$Recipient.Original.Postal.Code <- as.numeric(as.character(multipleoffenders$Recipient.Original.Postal.Code))
multipleoffenders <- mutate(multipleoffenders, Zip = ifelse((multipleoffenders$Recipient.Original.Postal.Code)-(multipleoffenders$Recipient.Postal.Code) != 0, "DIFFERENT", "SAME"))
##for quicker analysis, we can sort by which problems are caused by improper postal code
multipleoffenders <- arrange(multipleoffenders, desc(tot), Zip)
##we can also add columns to see which areas were similar and different
multipleoffenders$Address <- ifelse(multipleoffenders$Recipient.Address == multipleoffenders$Recipient.Original.Address, "SAME", "DIFFERENT")
multipleoffenders$Address.Line2 <- ifelse(multipleoffenders$Recipient.Address.Line2 == multipleoffenders$Recipient.Original.Address.Line2, "SAME", "DIFFERENT")
multipleoffenders$State <- ifelse(as.character(multipleoffenders$Recipient.State) == as.character(multipleoffenders$Recipient.Original.State), "SAME", "DIFFERENT")
multipleoffenders$City <- ifelse(as.character(multipleoffenders$Recipient.City) == as.character(multipleoffenders$Recipient.Original.City), "SAME", "DIFFERENT")

write.xlsx(multipleoffenders, "FedEx_Address_Correction_Analysis.xlsx", sheetName = "AddressDetails", append=TRUE)

##Counts over time
months <- ddply(addcorr, c("Invoice.date..yyyymm."), summarise, Count = sum(count))
#graph <- barplot(months$Count, names.arg=months$Invoice.date..yyyymm., main = "Address Corrections by Invoice Month")
write.xlsx(months, "FedEx_Address_Correction_Analysis.xlsx", sheetName = "CountbyMonth", append=TRUE)
