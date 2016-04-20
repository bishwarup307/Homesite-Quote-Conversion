require(readr)
require(caret)

setwd("F:/Kaggle/Homesite")
train <- read_csv("./Data/train.csv")

skf <- createFolds(train$QuoteConversion_Flag, k = 4, list = TRUE)

max.len <- max(sapply(skf, length))
folds <- data.frame(a = rep(-1, max.len))

for(i in 1:4) {
  
  idx <- skf[[i]]
  val <- train[idx,]
  ids <- val$QuoteNumber
  if (length(ids) < max.len) {
    
    ids <- c(ids, NA)
    
  }
  
  folds[[paste0("Fold_", i)]] <- ids
  
}

folds$a <- NULL
write_csv(folds, "./The Quants/validation_quotes.csv")
