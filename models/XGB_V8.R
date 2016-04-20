# laod required libraries
require(readr)
require(xgboost)
# set working directory & seed
setwd("F:/Kaggle/Homesite")
set.seed(3)
# load data
ptr <- read_csv("./Data/train_cat_coded_v10.csv")
pte <- read_csv("./Data/test_cat_coded_v10.csv")

feature.names <- names(ptr)[!names(ptr) %in% c("QuoteNumber", "Original_Quote_Date", "QuoteConversion_Flag", "split1",
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

dtrain <- xgb.DMatrix(data.matrix(ptr[, feature.names]), label = ptr$QuoteConversion_Flag)
watchlist <- list(train = dtrain)

bst <- xgb.train(data = dtrain,
                 params = param,
                 verbose=1,
                 nround = 3350,
                 print.every.n = 20,
                 maximize = TRUE,
                 watchlist = watchlist)

preds <- predict(bst, data.matrix(pte[, feature.names]))
df <- data.frame(QuoteNumber = pte$QuoteNumber, QuoteConversion_Flag = preds)
write_csv(df, "./Submissions/XGB_V8.csv")
