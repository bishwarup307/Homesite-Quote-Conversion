require(readr)
require(LiblineaR)
require(Metrics)

setwd("F:/Kaggle/Homesite")
set.seed(12)

load("./DataGLM_KNN/alldforglmknn.RData")
train <- alld[alld$trainFlag == 1,]
test <- alld[alld$trainFlag == 0,]

folds <- read_csv("./The Quants/validation_quotes.csv")

feature.names <- names(train)[!names(train) %in% c("QuoteNumber", "QuoteConversion_Flag", "trainFlag")]

evalMatrix <- data.frame(QuoteNumber = numeric(), svc_v1 = numeric())

for (i in 1:4){
  
  cat("\n---------------------------")
  cat("\n-------- CV: ", i, "-----------\n")
  cat("---------------------------\n")
  
  idx <- folds[[i]]
  idx <- idx[!is.na(idx)]
  trainingSet <- train[!train$QuoteNumber %in% idx,]
  validationSet <- train[train$QuoteNumber %in% idx,]
  
  cat("\n nrow train: ", nrow(trainingSet))
  cat("\n nrow eval: ", nrow(validationSet), "\n\n")
  
  svc <- LiblineaR(trainingSet[, feature.names],
                   as.factor(trainingSet$QuoteConversion_Flag),
                   type = 0,
                   cost = 10000,
                   epsilon = 1e-6,
                   verbose = TRUE)
  
  pred <- predict(svc, newx = validationSet[, feature.names], proba = TRUE)
  preds <- pred$probabilities[,2]
  df <- data.frame(QuoteNumber = validationSet$QuoteNumber, svc_v1 = preds)
  evalMatrix <- rbind(evalMatrix, df)  
}

write_csv(evalMatrix, "./The Quants/Validation Predictions/bishwarup_svc_1_validation.csv")


svc_all <- LiblineaR(train[, feature.names],
                 as.factor(train$QuoteConversion_Flag),
                 type = 0,
                 cost = 10000,
                 epsilon = 1e-6,
                 verbose = TRUE)

tpred <- predict(svc_all, newx = test[, feature.names], proba = TRUE)
tpreds <- tpred$probabilities[,2]
testDF <- data.frame(QuoteNumber = test$QuoteNumber, svc_v1 = tpreds)
write_csv(testDF, "./The Quants/Test Predictions/bishwarup_svc_1_test.csv")
