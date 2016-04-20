#########################################
### Kaggle: Homesite Quote Conversion ###
### author: bishwarup ###################
### data version :V1 ####################
#########################################

# load required libraries
require(readr)
require(lubridate)
require(ProgGUIinR)
require(dplyr)
require(Rtsne)
require(irlba)

# set working directory
setwd("~/Kaggle/Homesite")

# merge train and test for preprocessing
train <- read_csv("./Data/train.csv")
test <- read_csv("./Data/test.csv")
train$split1 <- 0
test$split1 <- 2
test$QuoteConversion_Flag <- (-1)
md <- rbind(train, test)

# extract date features
md$originalQuoteYear <- year(md$Original_Quote_Date)
md$originalQuoteMonth <- month(md$Original_Quote_Date)
md$originalQuoteDay <- mday(md$Original_Quote_Date)
md$originalQuoteyweek <- week(md$Original_Quote_Date)
md$originalQuotemweek <- week.of.month(md$originalQuoteYear, md$originalQuoteMonth, md$originalQuoteDay)

# factorize categorical features
k <- sapply(md, class)
d <- names(which(k == "character"))
sapply(md[,d], function(x) length(unique(x)))
md[,d] <- data.frame(lapply(md[,d], function(x) as.integer(as.factor(x))))

# K-Means clustering & TSNE of coverage features
coverageFields <- select(md, contains("Coverage"))
names(coverageFields)
coverageFields$coverageKM4 <- kmeans(coverageFields, centers = 4, iter.max = 50)$cluster
coverageFields$coverageKM8 <- kmeans(coverageFields, centers = 8, iter.max = 50)$cluster
coverageTSNE <- Rtsne(as.matrix(coverageFields[,1:16]), perplexity = 30, theta = 0.5, max_iter = 500, pca = FALSE, check_duplicates = FALSE, verbose = TRUE)
coverageFields <- cbind(coverageFields, coverageTSNE$Y)
write_csv(coverageFields, "./Data/coverageFields.csv")

# K-Means and stochastic PCA of sales fileds
salesFields <- select(md, contains("SalesField"))
salesFields$salesKM4 <- kmeans(salesFields, centers = 4, iter.max = 50)$cluster
salesFields$salesKM8 <- kmeans(salesFields, centers = 8, iter.max = 50)$cluster

sf <- data.matrix(salesFields[,1:17])
svd <- irlba(sf, nu = 0, nv = 4, center = colMeans(sf))
PC <- data.frame(sf %*% svd$v)
names(PC) = paste0("sales_PC_", 1:4)

salesFields <- cbind(salesFields, PC)
write_csv(salesFields, "./Data/salesFields.csv")

# K-Means and stochastic PCA of personal records of customers
personalFields <- select(md, contains("PersonalField"))
personalFields[is.na(personalFields)] <- -1

personalFields$personalKM4 <- kmeans(personalFields, centers = 4, iter.max = 50)$cluster
personalFields$personalKM8 <- kmeans(personalFields, centers = 8, iter.max = 50)$cluster
personalFields$personalKM12 <- kmeans(personalFields, centers = 12, iter.max = 50)$cluster

sf <- data.matrix(personalFields[,1:83])
svd <- irlba(sf, nu = 0, nv = 8, center = colMeans(sf))
PC <- data.frame(sf %*% svd$v)
names(PC) = paste0("personal_PC_", 1:8)

personalFields <- cbind(personalFields, PC)
write_csv(personalFields, "./Data/personalFields.csv")

# K-Means and stochastic PCA of peroperty of a quote
propertyFields <- select(md, contains("propertyField"))
sum(is.na(propertyFields))
sapply(propertyFields, function(x) sum(is.na(x)))
summary(propertyFields$PropertyField29)
propertyFields[is.na(propertyFields)] <- -1

propertyFields$propertyKM4 <- kmeans(propertyFields, centers = 4, iter.max = 50)$cluster
propertyFields$propertyKM8 <- kmeans(propertyFields, centers = 8, iter.max = 50)$cluster
propertyFields$propertyKM12 <- kmeans(propertyFields, centers = 12, iter.max = 50)$cluster

sf <- data.matrix(propertyFields[,1:47])
svd <- irlba(sf, nu = 0, nv = 6, center = colMeans(sf))
PC <- data.frame(sf %*% svd$v)
names(PC) = paste0("property_PC_", 1:6)

propertyFields <- cbind(propertyFields, PC)
write_csv(propertyFields, "./Data/propertyFields.csv")

# K-Means and stochastic PCA of geographical attributes
geoFields <- select(md, contains("GeographicField"))
sum(is.na(geoFields))

geoFields$geoKM4 <- kmeans(geoFields, centers = 4, iter.max = 50)$cluster
geoFields$geoKM8 <- kmeans(geoFields, centers = 8, iter.max = 50)$cluster
geoFields$geoKM12 <- kmeans(geoFields, centers = 12, iter.max = 50)$cluster

sf <- data.matrix(geoFields[,1:126])
svd <- irlba(sf, nu = 0, nv = 12, center = colMeans(sf))
PC <- data.frame(sf %*% svd$v)
names(PC) = paste0("geo_PC_", 1:12)

geoFields <- cbind(geoFields, PC)
write_csv(geoFields, "./Data/geoFields.csv")

# merge all the extracted features
separatedCols <- c(names(coverageFields), names(salesFields), names(personalFields),names(propertyFields), names(geoFields))
md <- md[, !names(md) %in% separatedCols]
md <- cbind(md, coverageFields)
md <- cbind(md, salesFields)
md <- cbind(md, personalFields)
md <- cbind(md, propertyFields)
md <- cbind(md, geoFields)

rm(coverageFields, salesFields, personalFields, propertyFields, geoFields, svd, PC, sf)

# K-Means of principal components
pcomp <- select(md, contains("_PC_"))
md$pcompKM2<- kmeans(pcomp, centers = 2, iter.max = 50)$cluster
md$pcompKM4 <- kmeans(pcomp, centers = 4, iter.max = 50)$cluster
md$pcompKM6 <- kmeans(pcomp, centers = 6, iter.max = 50)$cluster
md$pcompKM8 <- kmeans(pcomp, centers = 8, iter.max = 50)$cluster

#-----------------------------------------------------------
df <- select(md, contains("KM"))
tsne <- Rtsne(as.matrix(df), perplexity = 30, theta = 0.5, pca = FALSE, check_duplicates = FALSE, max_iter = 500, verbose = TRUE)
a <- data.frame(tsne$Y)
names(a) <- c("KM_TSNE_1", "KM_TSNE_2")
md <- cbind(md, a)
save(md, file = "./Data/homesite_v1.RData")

#------------------------------------------------------------

df <- select(md, contains("_PC_"))
tsne <- Rtsne(as.matrix(df), perplexity = 30, theta = 0.5, pca = FALSE, check_duplicates = FALSE, max_iter = 500, verbose = TRUE)
a <- data.frame(tsne$Y)
names(a) <- c("PCOMP_TSNE_1", "PCOMP_TSNE_2")
md <- cbind(md, a)
save(md, file = "./Data/homesite_v1.RData")

#-------------------------------------------------------------

write_csv(md, "./Data/homesite_v1.csv")