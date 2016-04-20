require(readr)
require(xgboost)
require(Metrics)

setwd("F:/Kaggle/Homesite")
set.seed(53)

load("./Data/alldata_coded_v7.RData")
ptr <- alldata[alldata$split1 == 0,]
pte <- alldata[alldata$split1 == 2,]
imp <- read_csv("./Data/Importance_matrix_for_cluster.csv")
valm <- read_csv("The Quants/validation_merged.csv")
testm <- read_csv("The Quants/test_merged.csv")


selectedCols <- c("QuoteNumber", "int_1", "SalesField5", "L2_int_4", "L2_int_2", "L3_int_2", "L3_int_1", "PersonalField9",
                  "PersonalField1", "PropertyField37", "PropertyField29", "int_7", "PersonalField10A", "PersonalField10B", "SalesField1A", 
                  "SalesField1B", "QuoteConversion_Flag", "PersonalField2", "PersonalField12", "Field7", "SalesField4", "PropertyField34",
                  "PersonalField13", "PersonalField27", "PersonalField82", "L2_int_6", "L2_int_5",
                  "CoverageField8", "int_11", "PersonalField26", "int_10", "SalesField6", "int_5", "PersonalField16_cat", "PersonalField17_cat",
                  "PersonalField18_cat", "PersonalField19_cat", "weekDay")

dd <- ptr[, selectedCols]
ddt <- pte[, selectedCols]

valm <- merge(valm, dd)
testm <- merge(testm, ddt)

feature.names <- names(valm)[!names(valm) %in% c("QuoteNumber", "QuoteConversion_Flag")]

param <- list(objective = "binary:logistic",
              eta = 0.015,
              max_depth = 6,
              subsample = 0.9,
              colsample_bytree = 0.7,
              min_child_weight = 4,
              eval_metric = "auc",
              gamma = 0.5)

dtrain <- xgb.DMatrix(data.matrix(valm[, feature.names]), label = valm$QuoteConversion_Flag)

watchlist <- list(train = dtrain)

bst <- xgb.train(data = dtrain,
                 params = param,
                 verbose=1,
                 nround = 950,
                 print.every.n = 50,
                 maximize = TRUE,
                 watchlist = watchlist)

preds <- predict(bst, data.matrix(testm[, feature.names]))
df <- data.frame(QuoteNumber = testm$QuoteNumber, QuoteConversion_Flag = preds)
write_csv(df, "./Submissions/stacked_4.csv")
