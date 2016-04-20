require(readr)
require(xgboost)

setwd("F:/Kaggle/Homesite")
set.seed(11)

load("./Data/paired_features.RData")
train <- md[md$split1 == 0,]
test <- md[md$split1 == 2,]
folds <- read_csv("./The Quants/validation_quotes.csv")

feature.names <- names(train)[!names(train) %in% c("QuoteNumber", "Original_Quote_Date", "QuoteConversion_Flag", "split1")]

param <- list(objective = "binary:logistic",
              eta = 0.02,
              max_depth = 6,
              subsample = 0.9,
              colsample_bytree = 0.55,
              min_child_weight = 5,
              eval_metric = "auc")

evalMatrix <- data.frame(QuoteNumber = numeric(), xgb_v2 = numeric())

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
                      nrounds             = 2350, 
                      verbose             = 1,
                      watchlist = watchlist,
                      print.every.n = 50,
                      maximize            = TRUE
  )
  
  preds <- predict(clf, data.matrix(validationSet[, feature.names]))
  df <- data.frame(QuoteNumber = validationSet$QuoteNumber, xgb_v2 = preds)
  evalMatrix <- rbind(evalMatrix, df)
  
}

dtrain <-  xgb.DMatrix(data = data.matrix(train[, feature.names]), label = train$QuoteConversion_Flag)
watchlist <- list(train = dtrain)

bst <- xgb.train(params = param,
                 data = dtrain,
                 nrounds = 2350,
                 verbose = 1,
                 watchlist = watchlist,
                 print.every.n = 50,
                 maximize = TRUE)


tpreds <- predict(bst, data.matrix(test[, feature.names]))
testDF <- data.frame(QuoteNumber = test$QuoteNumber, xgb_v2 = tpreds)

write_csv(evalMatrix, "./The Quants/Validation Predictions/bishwarup_xgb_2_validation.csv")
write_csv(testDF, "./The Quants/Test predictions/bishwarup_xgb_2_test.csv")