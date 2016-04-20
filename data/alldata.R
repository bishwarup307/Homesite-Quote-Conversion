#########################################
### Kaggle: Homesite Quote Conversion ###
### author: bishwarup ###################
### data version :V2 ####################
#########################################

require(readr)
require(lubridate)

setwd("F:/Kaggle/Homesite")

train <- read_csv("./Data/train.csv")
test <- read_csv("./Data/test.csv")

train$trainFlag <- 1
test$trainFlag <- 0
test$QuoteConversion_Flag <- NA

alldata <- rbind(train, test)
alldata$na_count <- apply(alldata[, c("PersonalField84", "PropertyField29")], 1, function(x) sum(is.na(x)))
alldata$diff_CoverageField1B_PropertyField21B <- alldata$CoverageField1B - alldata$PropertyField21B
alldata$diff_GeographicField6A_GeographicField8A <- alldata$GeographicField6A - alldata$GeographicField8A
alldata$diff_GeographicField6A_GeographicField13A <- alldata$GeographicField6A - alldata$GeographicField13A
alldata$diff_GeographicField8A_GeographicField13A <- alldata$GeographicField8A - alldata$GeographicField13A
alldata$diff_GeographicField11A_GeographicField13A <- alldata$GeographicField11A - alldata$GeographicField13A
alldata$diff_GeographicField8A_GeographicField11A <- alldata$GeographicField8A - alldata$GeographicField11A

alldata$month <- month(alldata$Original_Quote_Date)
alldata$year <- year(alldata$Original_Quote_Date)
alldata$mday <- mday(alldata$Original_Quote_Date)
alldata$weekDay <- wday(alldata$Original_Quote_Date)
alldata$Original_Quote_Date <- NULL

c = 0
for (f in names(alldata)) {
  
  if(class(alldata[[f]]) == "character") {
    
    alldata[[f]] <- as.integer(as.factor(alldata[[f]]))
    c = c+1
    cat("\n",c)
    
  }
}

alldata[is.na(alldata)] <- -1
save(alldata, file = "./Data/alldata.RData")
