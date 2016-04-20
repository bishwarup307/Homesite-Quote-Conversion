# -*- coding: utf-8 -*-
"""
Created on Fri Jan 22 23:49:51 2016
@author: Bishwarup
"""
import os
import pandas as pd
import numpy as np
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.metrics import roc_auc_score

print "Loading data .... "
os.chdir("F:/Kaggle/Homesite")

train = pd.read_csv("Data/train_rf.csv")
test = pd.read_csv("Data/test_rf.csv")
fold_ids = pd.read_csv("The Quants/validation_quotes.csv")

feature_names = [x for x in train.columns if x not in ["QuoteNumber", "Original_Quote_Date", "QuoteConversion_Flag", "split1"]]
test_X = np.matrix(test[feature_names])

et_val = pd.DataFrame(columns = ["QuoteNumber", "et_v1"])

for i in xrange(0, 4):

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
    
    et = ExtraTreesClassifier(n_estimators = 2000, criterion = "gini", max_features = 0.8,
                           max_depth = 25, min_samples_split = 20,
                           n_jobs = -1, random_state = 112, verbose = 1)
    
    et.fit(tr_X, tr_Y)                          
    preds = et.predict_proba(val_X)[:,1]
    df = pd.DataFrame({"QuoteNumber" : validationSet["QuoteNumber"], "et_v1" : preds})
    et_val = et_val.append(df, ignore_index = True)
    roc_auc_score(validationSet["QuoteConversion_Flag"], preds)

et_val.to_csv("The Quants/Validation Predictions/bishwarup_et_1_validation.csv", index = False)

tr_X = np.matrix(train[feature_names])
tr_Y = np.array(train["QuoteConversion_Flag"])

et = ExtraTreesClassifier(n_estimators = 2000, criterion = "gini", max_features = 0.8,
                       max_depth = 25, min_samples_split = 20,
                       n_jobs = -1, random_state = 112, verbose = 1)
                       
et.fit(tr_X, tr_Y)                       
tpreds = et.predict_proba(test_X)[:,1]
et_test = pd.DataFrame({"QuoteNumber" : test["QuoteNumber"], "et_v1" : tpreds})
et_test.to_csv("The Quants/Test Predictions/bishwarup_et_1_test.csv", index = False)
