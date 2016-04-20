# -*- coding: utf-8 -*-
"""
Created on Tue Jan 26 19:01:49 2016

@author: Bishwarup
"""

import os
import numpy as np
import pandas as pd
from sklearn.preprocessing import LabelEncoder, StandardScaler
from sklearn.metrics import roc_auc_score
from sklearn.neighbors import KNeighborsClassifier

os.chdir("F:/Kaggle/Homesite")
np.random.seed(135)

def load_data():
    
    train = pd.read_csv("Data/train.csv")
    test = pd.read_csv("Data/test.csv")
    train["train_flag"] = 1
    test["train_flag"] = 0    
    test["QuoteConversion_Flag"] = -1
    alldata = train.append(test, ignore_index = True)
    alldata.fillna(-1, inplace = True)    
    
    alldata["Date"] = pd.to_datetime(pd.Series(alldata['Original_Quote_Date']))
    alldata = alldata.drop("Original_Quote_Date", axis = 1)
    alldata["year"] = alldata["Date"].apply(lambda x: int(str(x)[:4]))
    alldata["month"] = alldata["Date"].apply(lambda x: int(str(x)[5:7]))
    alldata['weekday'] = [alldata['Date'][i].dayofweek for i in range(len(alldata['Date']))]
    
    alldata["int_1"] = alldata["CoverageField1B"] - alldata["PropertyField21B"]
    alldata["int_2"] = alldata["GeographicField6A"] - alldata["GeographicField8A"]
    alldata["int_3"] = alldata["GeographicField6A"] - alldata["GeographicField13A"]
    alldata["int_4"] = alldata["GeographicField8A"] - alldata["GeographicField13A"]
    alldata["int_5"] = alldata["GeographicField11A"] - alldata["GeographicField13A"]
    alldata["int_6"] = alldata["GeographicField8A"] - alldata["GeographicField11A"]
    alldata.drop("Date", axis = 1, inplace = True)
    cat_cols = [col for col in alldata.columns if alldata[col].dtype == "O"]
    
    if need_categorical:
        cat_cols = [col for col in alldata.columns if alldata[col].dtype == "O"]    
        cat_df = alldata[cat_cols]
        dummy_df = pd.get_dummies(cat_df, prefix_sep = "_")
        dummy_df[dummy_df == 0] = -1
        alldata.drop(cat_cols, axis = 1, inplace = True)
        alldata = pd.concat([alldata, dummy_df], axis = 1)
        
    else:
        for f in alldata.columns:
            if alldata[f].dtype == "object":
                lbl = LabelEncoder()
                alldata[f] = lbl.fit_transform(alldata[f])
                
    
    if need_normalize:
        
        if need_categorical:
            
            non_normalize_cols = ["QuoteNumber", "QuoteConversion_Flag", "train_flag"]
            cat_cols.extend(non_normalize_cols)
            normalize_cols = [col for col in alldata.columns if col not in non_normalize_cols]
            norm_df = alldata[normalize_cols]
            scaler = StandardScaler()
            norm_df = pd.DataFrame(scaler.fit_transform(norm_df))
            norm_df.columns = normalize_cols
            alldata.drop(normalize_cols, axis = 1, inplace = True)
            alldata = pd.concat([alldata, norm_df], axis = 1)
        else:
            non_normalize_cols = ["QuoteNumber", "QuoteConversion_Flag", "train_flag"]
            normalize_cols = [cols for cols in alldata.columns if cols not in non_normalize_cols]
            norm_df = alldata[normalize_cols]
            scaler = StandardScaler()
            norm_df = pd.DataFrame(scaler.fit_transform(norm_df))
            norm_df.columns = normalize_cols
            alldata.drop(normalize_cols, axis = 1, inplace = True)
            alldata = pd.concat([alldata, norm_df], axis = 1)
    
    ptr = alldata[alldata.train_flag == 1]
    pte = alldata[alldata.train_flag == 0]
    return ptr, pte


if __name__ == '__main__':
    
    need_categorical = False
    need_normalize = True
    fold_ids = pd.read_csv("The Quants/validation_quotes.csv")
    
    train, test = load_data()
    feature_names = [x for x in train.columns if x not in ["QuoteNumber", "QuoteConversion_Flag", "train_flag"]]
    knn_val = pd.DataFrame(columns = ["QuoteNumber", "knn_v1"])
    test_X = np.matrix(test[feature_names])
    
    for i in xrange(4):
    
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
        
        knn = KNeighborsClassifier(n_neighbors = 7,
                                   weights = "uniform",
                                   n_jobs = 11)
        knn.fit(tr_X, tr_Y)
        preds = knn.predict_proba(val_X)[:, 1]
        print "\nAUC", roc_auc_score(val_Y, preds)
        df = pd.DataFrame({"QuoteNumber" : validationSet["QuoteNumber"], "knn_v1" : preds})
        knn_val = knn_val.append(df, ignore_index = True)
        
    knn_val.to_csv("The Quants/Validation Predictions/bishwarup_knn_1_validation.csv", index = False)    
    
    tr_X = np.matrix(train[feature_names])
    tr_Y = np.array(train["QuoteConversion_Flag"])
    
    knn = KNeighborsClassifier(n_neighbors = 7,
                               weights = "uniform",
                               n_jobs = 11)
    
    knn.fit(tr_X, tr_Y)
    tpreds = knn.predict_proba(test_X)[:, 1]
    test_df = pd.DataFrame({"QuoteNumber" : test["QuoteNumber"], "knn_v1" : tpreds})
    test_df.to_csv("The Quants/Test Predictions/bishwarup_knn_1_test.csv", index = False)
