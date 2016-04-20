#########################################
### Kaggle: Homesite Quote Conversion ###
### author: bishwarup ###################
### data version :V7 ####################
#########################################
# load required libraries
require(readr)
require(Hmisc)
# set working directory
setwd("F:/Kaggle/Homesite")
# merge train and test for preprocessing
ptr <- read_csv("./Data/ptr_coded_v7.csv")
pte <- read_csv("./Data/pte_coded_v7.csv")
train <- read_csv("./Data/train.csv")
test <- read_csv("./Data/test.csv")
train$trainFlag <- 1
test$trainFlag <- 0
test$QuoteConversion_Flag <- NA
md <- rbind(train, test)

# basic feature engineering
md[is.na(md)] <- -1
md$Nac <- apply(md[, -c(1, 2, 3, 300)], 1, function(x) sum(x == -1))
coverage <- md[, grep("CoverageField", names(md), value = TRUE)]
sales <- md[, grep("SalesField", names(md), value = TRUE)]
personal <- md[, grep("PersonalField", names(md), value = TRUE)]
property <- md[, grep("PropertyField", names(md), value = TRUE)]
geo <- md[, grep("Geo", names(md), value = TRUE)]
coverage <- coverage[, !names(coverage) %in% names(which(sapply(coverage, class) == "character"))]
sales <- sales[, !names(sales) %in% names(which(sapply(sales, class) == "character"))]
personal <- personal[, !names(personal) %in% names(which(sapply(personal, class) == "character"))]
property <- property[, !names(property) %in% names(which(sapply(property, class) == "character"))]
geo <- geo[, !names(geo) %in% names(which(sapply(geo, class) == "character"))]

# derive sums of different segments of data
md$sumCoverage <- apply(coverage, 1, function(x) sum(x, na.rm = TRUE))
md$sumSales <- apply(sales, 1, function(x) sum(x, na.rm = TRUE))
md$sumPersonal <- apply(personal, 1, function(x) sum(x, na.rm = TRUE))
md$sumProperty <- apply(property, 1, function(x) sum(x, na.rm = TRUE))
md$sumGeo <- apply(geo, 1, function(x) sum(x, na.rm = TRUE))

alldata <- rbind(ptr, pte)
alldata$SalesField8_cat <- as.integer(Hmisc::cut2(alldata$SalesField8, cuts = seq(0.5, 11.5, by = 0.5)))

dd <- md[, c("QuoteNumber", "Nac", "sumCoverage", "sumSales", "sumPersonal", "sumProperty", "sumGeo")]
alldata <- merge(alldata, dd, by = "QuoteNumber", all.x = TRUE)

save(alldata, file = "./Data/alldata_coded_v7.RData")
