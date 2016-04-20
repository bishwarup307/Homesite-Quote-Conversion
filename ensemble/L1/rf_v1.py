# -*- coding: utf-8 -*-
"""
Spyder Editor

author: bishwarup
"""

import os
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import roc_auc_score


print "Loading data .... "
os.chdir("F:/Kaggle/Homesite")

train = pd.read_csv("Data/train_rf.csv")
test = pd.read_csv("Data/test_rf.csv")
fold_ids = pd.read_csv("The Quants/validation_quotes.csv")

feature_names = [x for x in train.columns if x not in ["QuoteNumber", "Original_Quote_Date", "QuoteConversion_Flag", "split1"]]
test_X = np.matrix(test[feature_names])

#rf_val = pd.DataFrame(columns = ["Id", "rf_v1"])

rf_val = pd.DataFrame({"QuoteNumber" : validationSet["QuoteNumber"], "rf_v1" : preds})
rf_test = pd.DataFrame(dict({"Id" : test["Id"]}))

for i in xrange(1, 4):

    print "\n--------------------------------------------"
    print "------------- Fold %d -----------------------" %i
    print "--------------------------------------------"
    
    val_ids = fold_ids.ix[:, i].dropna()
    idx = train["QuoteNumber"].isin(list(val_ids))
    
    trainingSet = train[~idx]
    validationSet = train[idx]
    
    tr_X = np.matrix(trainingSet[feature_names])
    tr_Y = np.array(trainingSet["QuoteConversion_Flag"])
    val_X = np.matrix(validationSet[feature_names])
    val_Y = np.array(validationSet["QuoteConversion_Flag"])
    
    rf = RandomForestClassifier(n_estimators = 2000, criterion = "gini", max_features = 0.8,
                           max_depth = 25, min_samples_split = 20,
                           n_jobs = -1, random_state = 112, verbose = 2)
    
    rf.fit(tr_X, tr_Y)                          
    preds = rf.predict_proba(val_X)[:,1]
    df = pd.DataFrame({"QuoteNumber" : validationSet["QuoteNumber"], "rf_v1" : preds})
    rf_val = rf_val.append(df, ignore_index = True)
    roc_auc_score(validationSet["QuoteConversion_Flag"], preds)

rf_val.to_csv("The Quants/Validation Predictions/bishwarup_rf_1_validation.csv", index = False)

tr_X = np.matrix(train[feature_names])
tr_Y = np.array(train["QuoteConversion_Flag"])

rf = RandomForestClassifier(n_estimators = 2000, criterion = "gini", max_features = 0.8,
                       max_depth = 25, min_samples_split = 20,
                       n_jobs = -1, random_state = 112, verbose = 1)
                       
rf.fit(tr_X, tr_Y)                       
tpreds = rf.predict_proba(test_X)[:,1]
rf_test = pd.DataFrame({"QuoteNumber" : test["QuoteNumber"], "rf_v1" : tpreds})
rf_test.to_csv("The Quants/Test Predictions/bishwarup_rf_1_test.csv", index = False)