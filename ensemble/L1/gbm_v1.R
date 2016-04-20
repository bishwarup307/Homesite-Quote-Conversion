require(readr)
require(gbm)

setwd("F:/Kaggle/Homesite")
set.seed(3)

train <- read_csv("./Data/train_rf.csv")
test <- read_csv("./Data/test_rf.csv")
folds <- read_csv("./The Quants/validation_quotes.csv")

feature.names <- names(train)[!names(train) %in% c("QuoteNumber", "Original_Quote_Date", "QuoteConversion_Flag", "split1")]

frm <- as.formula(paste0("QuoteConversion_Flag ~ ", paste(feature.names, collapse = "+")))

evalMatrix <- data.frame(QuoteNumber = numeric(), gbm_v1 = numeric())

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
  
  
  gbm_fit <- gbm(frm,
                 data = trainingSet,
                 distribution = "bernoulli",
                 n.trees = 2000,
                 interaction.depth = 7,
                 shrinkage = 0.03,
                 bag.fraction = 0.85,
                 n.minobsinnode = 15,
                 verbose = TRUE,
                 n.cores = 11)
  
  preds <- predict(gbm_fit, validationSet[, feature.names], n.trees = 2000)
  df <- data.frame(QuoteNumber = validationSet$QuoteNumber, gbm_v1 = preds)
  evalMatrix <- rbind(evalMatrix, df)
  
}

full_gbm <- gbm(frm,
                data = train,
                distribution = "bernoulli",
                n.trees = 2000,
                interaction.depth = 7,
                shrinkage = 0.03,
                bag.fraction = 0.85,
                n.minobsinnode = 15,
                verbose = TRUE,
                n.cores = 11)

tpreds <- predict(full_gbm, test[, feature.names], n.trees = 2000)
testDF <- data.frame(QuoteNumber = test$QuoteNumber, gbm_v1 = tpreds)

evalMatrix$gbm_v1 <- 1/(1 + exp(-evalMatrix$gbm_v1))
testDF$gbm_v1 <- 1/(1 + exp(-testDF$gbm_v1))

write_csv(evalMatrix, "./The Quants/Validation Predictions/bishwarup_gbm_1_validation.csv")
write_csv(testDF, "./The Quants/Test predictions/bishwarup_gbm_1_test.csv")

