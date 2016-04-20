#########################################
### Kaggle: Homesite Quote Conversion ###
### author: bishwarup ###################
### data version :for GLM_KNN_eNET ######
#########################################


## load required libraries
require(readr)
require(lubridate)
require(caret)

# set working directory
setwd("F:/Kaggle/Homesite")
set.seed(1)

# merge train and test for preprocessing
train <- read_csv("./Data/train.csv")
test <- read_csv("./Data/test.csv")
train$trainFlag <- 1
test$trainFlag <- 0
test$QuoteConversion_Flag <- NA
alldata <- rbind(train, test)

# remove constant features
alldata <- alldata[, !names(alldata) %in% names(which(sapply(alldata[-c(1:3)], function(x) length(unique(x))) == 1))]
alldata[is.na(alldata)] <- -1

# extract date features
alldata$year <- year(alldata$Original_Quote_Date)
alldata$month <- month(alldata$Original_Quote_Date)
alldata$weekday <- wday(alldata$Original_Quote_Date)
alldata$day <- mday(alldata$Original_Quote_Date)
alldata$Original_Quote_Date <- NULL

# replace NULL categories with -1
catCols <- names(which(sapply(alldata[, -c(1, 2)], class) == "character"))
for (f in catCols) {
  
  alldata[[f]][alldata[[f]] == ""] <- "-1"
  
}

alldata$na_count <- apply(alldata[, -c(1, 2)], 1, function(x) sum(x == -1 | x== "-1"))

# dummy code the categoricals
catDF <- alldata[, catCols]
dmy <- dummyVars("~.", data = catDF)
trsf <- data.frame(predict(dmy, newdata = catDF))
trsf <- trsf[, !names(trsf) %in% names(which(sapply(trsf, sum) < 100))]
alldata <- alldata[, !names(alldata) %in% catCols]

# scale(log+1) trafo
sc <- log(alldata[, -c(1,2,271)] + 2)
sc <- as.data.frame(scale(sc))

alld <- cbind(alldata[, c("QuoteNumber", "QuoteConversion_Flag", "trainFlag")], sc)
alld <- cbind(alld, trsf)
save(alld, file = "./DataGLM_KNN/alldforglmknnlog.RData")
ptr <- alld[alld$trainFlag == 1,]
pte <- alld[alld$trainFlag == 0,]

# save to disk
write_csv(ptr, "./DataGLM_KNN/train_glm_knn_log.csv")
write_csv(pte, "./DataGLM_KNN/test_glm_knn_log.csv")
