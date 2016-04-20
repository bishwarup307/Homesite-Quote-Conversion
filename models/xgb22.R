# laod required libraries
require(readr)
require(xgboost)
# set working directory & seed
setwd("~/Kaggle/Homesite")
set.seed(3251)
# load data
ptr <- read_csv("./Data/ptr_v7_coded_stacked_RFGINI_ETGINI.csv")
pte <- read_csv("./Data/pte_v7_coded_stacked_RFGINI_ETGINI.csv")

feature.names <- names(ptr)[!names(ptr) %in% c("QuoteNumber", "QuoteConversion_Flag", "Original_Quote_Date", "split1",
                                               "PersonalField16", "PersonalField17", "PersonalField18", "PersonalField19")]

# xgb parameters
param <- list(objective = "binary:logistic",
              eta = 0.008,
              max_depth = 8,
              subsample = 0.8,
              colsample_bytree = 0.64,
              min_child_weight = 6,
              eval_metric = "auc")

dtrain <- xgb.DMatrix(data = data.matrix(ptr[, feature.names]), label = ptr$QuoteConversion_Flag)
watchlist <- list(train = dtrain)
bst <- xgb.train(data = dtrain,
                 params = param,
                 verbose=1,
                 nround=3900,
                 print.every.n = 20,
                 maximize = TRUE,
                 watchlist = watchlist)

pred <- predict(bst, data.matrix(pte[, feature.names]))
sub <- data.frame(QuoteNumber = pte$QuoteNumber, QuoteConversion_Flag = pred)
write_csv(sub, "./Submissions/xgb_run_22.csv")
