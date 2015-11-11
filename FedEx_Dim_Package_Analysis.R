##FedEx Dimmed Package Detail Analysis
##Michael Green

##Download your report from your fedex reporting home for whatever time frame you desire
##Save it to the desktop

setwd("~/Desktop")
library("dplyr")
library("plyr")
library("xlsx")
library("sendmailR")

##Load file NOTE: you need to enter your file's name here
dims <- read.csv("FedEx_Dimmed_Package_Detail_Payer_Detail_2625500.csv", header = TRUE)
colnames(dims)
dims <- mutate(dims, count = 1)

##see which package dimensions are the most commonly subject to dimensional weight charges
packs <- ddply(dims, c("Dimmed.height", "Dimmed.weight", "Dimmed.length"), summarise, Count = sum(count))


#email results
#to <- "michael.green@%ofmodern.com"
#from <-"michael.green@%ofmodern.com"
#subject <- "testpacks"
#body <- head(packs)
#sendmail(from, to, subject, body, control= list(smtpServer="Smtp.gmail.com"))

## find average Original.weight of the package dimensions subject to dim charges
t <- function(x, f) {
  e <- numeric(nrow(f))
  for (i in 1:nrow(f)) {
    t <- filter(x, Dimmed.height == f[i,1], Dimmed.width == f[i,2], Dimmed.length == f[i,3])
    e[i] <- mean(t$Original.weight)
  }
  return(e)
}
avgweight <- t(dims, packs)
packs <- cbind(packs, avgweight)
## add dim weight charged on packages
packs <- mutate(packs, dimweight = ceiling((Dimmed.height*Dimmed.width*Dimmed.length/164)))

##print to excel
write.xlsx(packs, "FedEx_Dim_Analysis.xlsx", sheetName = "CountByPackageDims")