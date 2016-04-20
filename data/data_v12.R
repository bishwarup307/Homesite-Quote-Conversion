#########################################
### Kaggle: Homesite Quote Conversion ###
### author: bishwarup ###################
### data version :V1 ####################
#########################################
# load required libraries
require(readr)
require(Hmisc)
require(dplyr)
require(Rtsne)
require(lubridate)
options(dplyr.print_max = Inf)
options(dplyr.width = Inf)

# set working directory
setwd("F:/Kaggle/Homesite")
source("./Scripts/final_utils__.R")

# merge train and test for preprocessing
train <- read_csv("./Data/train.csv")
test <- read_csv("./Data/test.csv")
train$trainFlag <- 1
test$trainFlag <- 0
test$QuoteConversion_Flag <- NA
alldata <- rbind(train, test)
# extract date features
alldata$weekDay <- wday(alldata$Original_Quote_Date)              ########################################
alldata$month <- month(alldata$Original_Quote_Date)               ########## Date operations  ############
alldata$qt <- quarter(alldata$Original_Quote_Date)                ########################################
alldata$year <- year(alldata$Original_Quote_Date)
alldata$Original_Quote_Date <- NULL

charCols <- names(which(sapply(alldata, class) == "character"))
discreteCols <- names(alldata)[!names(alldata) %in% c("QuoteNumber", "QuoteConversion_Flag", "Field8", "Field9", "Field10", " Field11", charCols,
                                                      "SalesField8", "month")]


a <- alldata[, discreteCols]
highCardinal <- names(which(sapply(a, function(x) length(unique(x))) > 10))
p <- names(which(sapply(a[, highCardinal], function(x) min(table(x[x!=-1], useNA = "ifany"))) < 100))
lowFreq <- grep("Sales|Personal", p, value = TRUE)
lowFreq <- lowFreq[lowFreq != "SalesField6"]


alldata[is.na(alldata)] <- -1

alldata$sumNA <- apply(alldata[, -c(1, 2, 299)], 1, function(x) sum(x == -1))   # NA count
alldata$sumZero <- apply(alldata[, -c(1, 2, 299)], 1, function(x) sum(x == 0))  # 0 count

alldata$SalesField8_cat <- as.integer(Hmisc::cut2(log(alldata$SalesField8), cuts = seq(0.5, 11.5, by = 0.5)))

for (f in lowFreq) {
  
  cat("\n Processsing columns: ", f)
  alldata[[f]] <- clipFreq(f) 
  
}

######## ## combine low frequency categories of selected features ############
alldata$SalesField5[alldata$SalesField5 == 2] <- 1
alldata$PropertyField7[alldata$PropertyField7 %in% c("C", "M", "P", "L", "F", "T")] <- "B"
alldata$SalesField6 <- ifelse(alldata$SalesField6 == 2, 1,
                              ifelse(alldata$SalesField6 == 6, 5,
                                     ifelse(alldata$SalesField6 == 9, 8,
                                            ifelse(alldata$SalesField6 %in% c(12, 13), 11,
                                                   ifelse(alldata$SalesField6 %in% c(19, 20), 18, alldata$SalesField6)))))

alldata$PropertyField10[alldata$PropertyField10 %in% c(2,3,4)] <- 0
alldata$PropertyField12[alldata$PropertyField12 %in% c(6, 7)] <- 5
alldata$PropertyField14[alldata$PropertyField14 == "D"] <- "C"
alldata$PropertyField15[alldata$PropertyField15 %in% c(6, 7, 12, 13, 14, 15)] <- 3
alldata$PropertyField17[alldata$PropertyField17 > 3] <- 3
alldata$PropertyField18[alldata$PropertyField18 > 5] <- 5
alldata$PropertyField19[alldata$PropertyField19 > 2] <- 2
alldata$PropertyField23[alldata$PropertyField23 %in% c(7, 8, 14)] <- 6
alldata$PropertyField25[alldata$PropertyField25 %in% c(3, 3.5, 4, 5, 6, 7)] <- 0
alldata$PropertyField27[alldata$PropertyField27 %in% c(2, 5, 7, 8, 9, 11, 12, 18, 20, 16)] <- 1

# group wise sum of features
coverage <- alldata[, grep("CoverageField", names(alldata), value = TRUE)]
sales <- alldata[, grep("SalesField", names(alldata), value = TRUE)]
personal <- alldata[, grep("PersonalField", names(alldata), value = TRUE)]
property <- alldata[, grep("PropertyField", names(alldata), value = TRUE)]
geo <- alldata[, grep("Geo", names(alldata), value = TRUE)]
coverage <- coverage[, !names(coverage) %in% names(which(sapply(coverage, class) == "character"))]
sales <- sales[, !names(sales) %in% names(which(sapply(sales, class) == "character"))]
personal <- personal[, !names(personal) %in% names(which(sapply(personal, class) == "character"))]
property <- property[, !names(property) %in% names(which(sapply(property, class) == "character"))]
geo <- geo[, !names(geo) %in% names(which(sapply(geo, class) == "character"))]

alldata$sumCoverage <- apply(coverage, 1, function(x) sum(x, na.rm = TRUE))
alldata$sumSales <- apply(sales, 1, function(x) sum(x, na.rm = TRUE))
alldata$sumPersonal <- apply(personal, 1, function(x) sum(x, na.rm = TRUE))
alldata$sumProperty <- apply(property, 1, function(x) sum(x, na.rm = TRUE))
alldata$sumGeo <- apply(geo, 1, function(x) sum(x, na.rm = TRUE))

# feature extraction
alldata$int_1 <- alldata$Field11 - (alldata$Field8 + alldata$Field9)
alldata$int_2 <- alldata$Field8/alldata$Field11
alldata$int_3 <- alldata$CoverageField2A - alldata$CoverageField1A
alldata$int_4 <- alldata$CoverageField2B - alldata$CoverageField1B
alldata$int_5 <- alldata$CoverageField2A - alldata$CoverageField3A
alldata$int_6 <- alldata$CoverageField3B - alldata$CoverageField2B
alldata$int_7 <- alldata$SalesField4 - alldata$SalesField5
alldata$int_8 <- alldata$PropertyField26B - alldata$PropertyField26A
alldata$int_9 <- alldata$PropertyField18 - alldata$PropertyField19
alldata$int_10 <- ifelse(alldata$PropertyField32 == "Y" & alldata$PropertyField34 == "Y" & alldata$PropertyField36 == "N" & alldata$PropertyField37 == "Y", 1, 0)
alldata$int_11 <- ifelse(alldata$PropertyField32 == "Y" & alldata$PropertyField34 == "Y" & alldata$PropertyField36 == "N" & alldata$PropertyField37 == "N", 1, 0)

alldata$L2_int_1 <- paste(alldata$PropertyField37, alldata$SalesField5, sep = "_")
alldata$L2_int_2 <- paste(alldata$PropertyField37, alldata$PersonalField9, sep = "_")
alldata$L2_int_3 <- paste(alldata$PropertyField37, alldata$PersonalField2, sep = "_")
alldata$L2_int_4 <- paste(alldata$SalesField5, alldata$PersonalField9, sep = "_")
alldata$L2_int_5 <- paste(alldata$SalesField5, alldata$PersonalField2, sep = "_")
alldata$L2_int_6 <- paste(alldata$PersonalField9, alldata$PersonalField2, sep = "_")
alldata$L2_int_7 <- paste(alldata$SalesField1A, alldata$SalesField1B, sep = "_")
alldata$L2_int_8 <- paste(alldata$PersonalField12, alldata$PersonalField13, sep = "_")
alldata$L2_int_9 <- paste(alldata$PersonalField12, alldata$PersonalField27, sep = "_")
alldata$L2_int_10 <- paste(alldata$PersonalField13, alldata$PersonalField27, sep = "_")
alldata$L2_int_11 <- paste(alldata$SalesField5, alldata$SalesField6, sep = "_")
alldata$L2_int_12 <- paste(alldata$PropertyField1B, alldata$PropertyField2B, sep = "_")
alldata$L2_int_13 <- paste(alldata$PersonalField4A, alldata$PersonalField4B, sep = "_")
alldata$L2_int_14 <- paste(alldata$SalesField2A, alldata$SalesField2B, sep = "_")
alldata$L2_int_15 <- paste(alldata$PersonalField10A, alldata$PersonalField10B, sep = "_")
alldata$L2_int_16 <- paste(alldata$CoverageField8, alldata$CoverageField9, sep = "_")
alldata$L2_int_17 <- paste(alldata$PropertyField39A, alldata$PropertyField39B, sep = "_")
alldata$L2_int_18 <- paste(alldata$PersonalField26, alldata$PersonalField27, sep = "_")


alldata$L3_int_1 <- paste(alldata$PropertyField37, alldata$SalesField5, alldata$PersonalField9, sep = "_")
alldata$L3_int_2 <- paste(alldata$PropertyField37, alldata$SalesField5, alldata$PropertyField29, sep = "_")
alldata$L3_int_3 <- paste(alldata$PersonalField12, alldata$PersonalField13, alldata$PersonalField27, sep = "_")
alldata$L3_int_4 <- paste(alldata$SalesField1A, alldata$SalesField1B, alldata$SalesField5, sep = "_")
alldata$L3_int_5 <- paste(alldata$PersonalField1, alldata$PersonalField2, alldata$PersonalField29, sep = "_")

alldata$new1 <- paste(alldata$CoverageField11A, alldata$CoverageField11B, sep = "_")
alldata$new2 <- paste(alldata$GeographicField1A, alldata$GeographicField1B, sep = "_")
alldata$new3 <- paste(alldata$Field7, alldata$Field8, sep = "_")
alldata$new4 <- paste(alldata$Field8, alldata$Field9, sep = "_")
alldata$new5 <- paste(alldata$CoverageField2B, alldata$CoverageField3B, sep = "_")
alldata$new6 <- paste(alldata$CoverageField1A, alldata$CoverageField3B, sep = "_")
alldata$new7 <- paste(alldata$CoverageField5A, alldata$CoverageField5B, sep = "_")
alldata$new8 <- paste(alldata$SalesField1A, alldata$SalesField4, sep = "_")
alldata$new9 <- paste(alldata$PersonalField1, alldata$PersonalField10A, sep = "_")
alldata$new10 <- paste(alldata$SalesField3, alldata$SalesField4, sep = "_")
alldata$new11 <- paste(alldata$SalesField4, alldata$SalesField5, sep = "_")
alldata$new12 <- paste(alldata$PropertyField34, alldata$SalesField5, sep = "_")
alldata$new13 <- paste(alldata$PersonalField9, alldata$PropertyField29, sep = "_")
alldata$new14 <- paste(alldata$PersonalField12, alldata$SalesField6, sep = "_")
alldata$new15 <- paste(alldata$Field7, alldata$Field6, sep = "_")
alldata$new16 <- paste(alldata$PersonalField82, alldata$PersonalField84, sep = "_")
alldata$new17 <- paste(alldata$PersonalField82, alldata$PropertyField35, sep = "_")
alldata$new18 <- paste(alldata$CoverageField8, alldata$Field7, sep = "_")
alldata$new19 <- paste(alldata$SalesField10, alldata$SalesField2B, sep = "_")
alldata$new20 <- paste(alldata$SalesField7, alldata$CoverageField11B, sep = "_")

# treating categorical features 
catCols <- names(which(sapply(train, class) == "character"))
catCols <- catCols[!catCols %in% c("PersonalField16", "PersonalField17", "PersonalField18", "PersonalField19")]

for (f in catCols) {
  cat("\n Processing column: ", f)
  alldata[[f]] <- as.integer(as.factor(alldata[[f]]))
  
}


# TSNE of geographical features
geo_tsne <- Rtsne(as.matrix(geo), dims = 2, perplexity = 30, theta = 0.5, check_duplicates = FALSE, pca = FALSE, verbose = TRUE)
gt <- data.frame(geo_tsne$Y)
names(gt) <- c("geo_TSNE_1", "geo_TSNE_2")
alldata <- cbind(alldata, gt)

# train/test split
ptr <- alldata[alldata$trainFlag == 1,]
pte <- alldata[alldata$trainFlag == 0,]

# save to disk
write_csv(ptr, "./data_v12/ptr_to_be_coded_v12.csv")
write_csv(pte, "./data_v12/pte_to_be_coded_v12.csv")
