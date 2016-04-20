require(readr)
require(glmnet)
require(Metrics)

setwd("F:/Kaggle/Homesite")
set.seed(12)

load("./DataGLM_KNN/alldforglmknn.RData")
train <- alld[alld$trainFlag == 1,]
test <- alld[alld$trainFlag == 0,]
folds <- read_csv("./The Quants/validation_quotes.csv")

feature.names <- names(train)[!names(train) %in% c("QuoteNumber", "QuoteConversion_Flag", "trainFlag")]

evalMatrix <- data.frame(QuoteNumber = numeric(), gnet_v1 = numeric())

for(i in 2:4) {
  
  cat("\n---------------------------")
  cat("\n-------- CV: ", i, "-----------\n")
  cat("---------------------------\n")
  
  idx <- folds[[i]]
  idx <- idx[!is.na(idx)]
  trainingSet <- train[!train$QuoteNumber %in% idx,]
  validationSet <- train[train$QuoteNumber %in% idx,]
  
  cat("\n nrow train: ", nrow(trainingSet))
  cat("\n nrow eval: ", nrow(validationSet), "\n\n")
  
  gnet <- glmnet(as.matrix(trainingSet[, feature.names]),
                 as.factor(trainingSet$QuoteConversion_Flag),
                 family = "binomial",
                 alpha = 0.08,
                 lambda.min.ratio = 1e-7,
                 nlambda = 500,
                 standardize = FALSE)
  
  preds <- predict(gnet, newx=as.matrix(validationSet[, feature.names]), s= 1e-7, type="response")[,1]
  
  df <- data.frame(QuoteNumber = validationSet$QuoteNumber, gnet_v1 = preds)
  evalMatrix <- rbind(evalMatrix, df)
  
}
write_csv(evalMatrix, "./The Quants/Validation Predictions/bishwarup_gnet_1_validation.csv")


gnet_full <-  glmnet(as.matrix(train[, feature.names]),
                     as.factor(train$QuoteConversion_Flag),
                     family = "binomial",
                     alpha = 0.08,
                     lambda.min.ratio = 1e-7,
                     nlambda = 500,
                     standardize = FALSE)

tpreds <- predict(gnet_full, newx=as.matrix(test[, feature.names]), s= 1e-7, type="response")[,1]
testDF <- data.frame(QuoteNumber = test$QuoteNumber, gnet_v1 = tpreds)
write_csv(testDF, "./The Quants/Test predictions/bishwarup_gnet_1_test.csv")
