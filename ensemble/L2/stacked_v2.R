# load libraries
require(readr)
require(xgboost)
require(Metrics)

# set seed and wd
setwd("F:/Kaggle/Homesite")
set.seed(53)

# load data
load("./Data/alldata_coded_v7.RData")
ptr <- alldata[alldata$split1 == 0,]
pte <- alldata[alldata$split1 == 2,]
imp <- read_csv("./Data/Importance_matrix_for_cluster.csv")
# read meta-features
valm <- read_csv("The Quants/validation_merged.csv")
testm <- read_csv("The Quants/test_merged.csv")

# L2 feature selection
selectedCols <- c("QuoteNumber", "int_1", "SalesField5", "L2_int_4", "L2_int_2", "L3_int_2", "L3_int_1", "PersonalField9",
                  "PersonalField1", "PropertyField37", "PropertyField29", "int_7", "PersonalField10A", "PersonalField10B", "SalesField1A", 
                  "SalesField1B", "QuoteConversion_Flag")

dd <- ptr[, selectedCols]
ddt <- pte[, selectedCols]
valm <- merge(valm, dd)
testm <- merge(testm, ddt)

feature.names <- names(valm)[!names(valm) %in% c("QuoteNumber", "QuoteConversion_Flag")]

param <- list(objective = "binary:logistic",
              eta = 0.01,
              max_depth = 5,
              subsample = 0.9,
              colsample_bytree = 0.7,
              min_child_weight = 3,
              eval_metric = "auc")

dtrain <- xgb.DMatrix(data.matrix(valm[, feature.names]), label = valm$QuoteConversion_Flag)
watchlist <- list(train = dtrain)
bst <- xgb.train(data = dtrain,
                 params = param,
                 verbose=1,
                 nround = 1330,
                 print.every.n = 50,
                 maximize = TRUE,
                 watchlist = watchlist)

preds <- predict(bst, data.matrix(testm[, feature.names]))
df <- data.frame(QuoteNumber = testm$QuoteNumber, QuoteConversion_Flag = preds)
write_csv(df, "./Submissions/stacked_2.csv")
