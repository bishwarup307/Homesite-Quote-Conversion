require(readr)
require(xgboost)

setwd("F:/Kaggle/Homesite")
set.seed(3)

load("./Data/alldata_coded_v7.RData")
folds <- read_csv("./The Quants/validation_quotes.csv")
train <- alldata[alldata$split1 == 0,]
test <- alldata[alldata$split1 == 2,]

feature.names <- names(train)[!names(train) %in% c("QuoteNumber", "Original_Quote_Date", "QuoteConversion_Flag", "split1",
                                               "PersonalField16", "PersonalField17", "PersonalField18", "PersonalField19",
                                               "PCOMP_TSNE_1", "PCOMP_TSNE_2", "PF16_PF19_tw", "PF17_PF19_tw", "PF18_PF19_tw", "PF16_PF17_PF18_thw",
                                               "PF16_PF18_PF19_thw", "PF17_PF18_PF19_thw", "PF16_PF17_tw", "PF16_PF18_tw", "PF17_PF18_tw",
                                               "ex_TSNE_1", "ex_TSNE_2", "qt", "coverage_int_4", "coverage_int_5")]

param <- list(objective = "binary:logistic",
              eta = 0.015,
              max_depth = 6,
              subsample = 0.9,
              colsample_bytree = 0.7,
              min_child_weight = 12,
              eval_metric = "auc")

evalMatrix <- data.frame(QuoteNumber = numeric(), xgb_v3 = numeric())

for(i in 1:4) {
  
  cat("\n---------------------------")
  cat("\n----- CV: ", i, "--------\n")
  cat("---------------------------\n")
  
  idx <- folds[[i]]
  idx <- idx[!is.na(idx)]
  trainingSet <- train[!train$QuoteNumber %in% idx,]
  validationSet <- train[train$QuoteNumber %in% idx,]
  
  cat("\n nrow train: ", nrow(trainingSet))
  cat("\n nrow eval: ", nrow(validationSet), "\n\n")
  
  dtrain <- xgb.DMatrix(data = data.matrix(trainingSet[, feature.names]), label = trainingSet$QuoteConversion_Flag)
  dval <- xgb.DMatrix(data = data.matrix(validationSet[, feature.names]), label = validationSet$QuoteConversion_Flag)
  
  watchlist <- list(eval = dval, train = dtrain)
  
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 3580, 
                      verbose             = 1,
                      watchlist = watchlist,
                      print.every.n = 50,
                      maximize            = TRUE
  )
  
  preds <- predict(clf, data.matrix(validationSet[, feature.names]))
  df <- data.frame(QuoteNumber = validationSet$QuoteNumber, xgb_v3 = preds)
  evalMatrix <- rbind(evalMatrix, df)
  
}

dtrain <-  xgb.DMatrix(data = data.matrix(train[, feature.names]), label = train$QuoteConversion_Flag)
watchlist <- list(train = dtrain)

bst <- xgb.train(params = param,
                 data = dtrain,
                 nrounds = 3580,
                 verbose = 1,
                 watchlist = watchlist,
                 print.every.n = 50,
                 maximize = TRUE)


tpreds <- predict(bst, data.matrix(test[, feature.names]))
testDF <- data.frame(QuoteNumber = test$QuoteNumber, xgb_v3 = tpreds)

write_csv(evalMatrix, "./The Quants/Validation Predictions/bishwarup_xgb_3_validation.csv")
write_csv(testDF, "./The Quants/Test predictions/bishwarup_xgb_3_test.csv")

