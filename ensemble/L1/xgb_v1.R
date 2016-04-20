#load required libraries
library(readr)
library(xgboost)

# set working directory and seed
setwd("F:/Kaggle/Homesite")
set.seed(1718)

# load data
cat("reading the train and test data\n")
train <- read_csv("./Data/train.csv")
test  <- read_csv("./Data/test.csv")
folds <- read_csv("./The Quants/validation_quotes.csv")

# There are some NAs in the integer columns so conversion to zero
train[is.na(train)]   <- -1
test[is.na(test)]   <- -1

# seperating out the elements of the date column for the train set
train$month <- as.integer(format(train$Original_Quote_Date, "%m"))
train$year <- as.integer(format(train$Original_Quote_Date, "%y"))
train$day <- weekdays(as.Date(train$Original_Quote_Date))

# removing the date column
train <- train[,-c(2)]

# seperating out the elements of the date column for the train set
test$month <- as.integer(format(test$Original_Quote_Date, "%m"))
test$year <- as.integer(format(test$Original_Quote_Date, "%y"))
test$day <- weekdays(as.Date(test$Original_Quote_Date))

# removing the date column
test <- test[,-c(2)]


feature.names <- names(train)[c(3:301)]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

# train prediction container
evalMatrix <- data.frame(QuoteNumber = numeric(), xgb_v1 = numeric())

param <- list(  objective           = "binary:logistic", 
                eval_metric         = "auc",
                eta                 = 0.023, 
                max_depth           = 6, 
                subsample           = 0.83,
                colsample_bytree    = 0.77
)

# train on folds - generate train meta features
for(i in 1:4) {
  
  cat("\n----- CV: ", i, "--------\n")
  
  idx <- folds[[i]]
  idx <- idx[!is.na(idx)]
  trainingSet <- train[!train$QuoteNumber %in% idx,]
  validationSet <- train[train$QuoteNumber %in% idx,]
  
  cat("\n nrow train: ", nrow(trainingSet))
  cat("\n nrow eval: ", nrow(validationSet), "\n")
  
  dtrain <- xgb.DMatrix(data = data.matrix(trainingSet[, feature.names]), label = trainingSet$QuoteConversion_Flag)
  dval <- xgb.DMatrix(data = data.matrix(validationSet[, feature.names]), label = validationSet$QuoteConversion_Flag)
  
  watchlist <- list(eval = dval, train = dtrain)
  
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 1800, 
                      verbose             = 1,
                      watchlist           = watchlist,
                      print.every.n       = 50,
                      maximize            = TRUE
  )
  
  preds <- predict(clf, data.matrix(validationSet[, feature.names]))
  df <- data.frame(QuoteNumber = validationSet$QuoteNumber, xgb_v1 = preds)
  evalMatrix <- rbind(evalMatrix, df)
  
}
# train on whole sample - generate test meta features
dtrain <-  xgb.DMatrix(data = data.matrix(train[, feature.names]), label = train$QuoteConversion_Flag)
watchlist <- list(train = dtrain)

bst <- xgb.train(params = param,
                 data = dtrain,
                 nrounds = 1800,
                 verbose = 1,
                 watchlist = watchlist,
                 print.every.n = 50,
                 maximize = TRUE)


tpreds <- predict(bst, data.matrix(test[, feature.names]))
testDF <- data.frame(QuoteNumber = test$QuoteNumber, xgb_v1 = tpreds)

# save the train and test predictions
write_csv(evalMatrix, "./The Quants/Validation Predictions/bishwarup_xgb_1_validation.csv")
write_csv(testDF, "./The Quants/Test predictions/bishwarup_xgb_1_test.csv")
