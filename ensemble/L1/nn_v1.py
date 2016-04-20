# -*- coding: utf-8 -*-
"""
Created on Sat Jan 23 11:43:08 2016

@author: Bishwarup
"""

import os
import numpy as np
import pandas as pd
from sklearn.preprocessing import LabelEncoder, StandardScaler
from sklearn.metrics import roc_auc_score

import theano
from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation
from keras.layers.normalization import BatchNormalization
from keras.layers.advanced_activations import PReLU
from keras.utils import np_utils
from keras.layers.advanced_activations import PReLU
from keras.optimizers import SGD, Adadelta, Adagrad
from keras.optimizers import Adagrad,SGD,Adadelta
from keras.callbacks import Callback

os.chdir("F:/Kaggle/Homesite")
np.random.seed(112)

need_normalize = True
need_categorical = False
need_validation = False
nb_epoch = 25
batch_size = 256

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

class MonitorAUC(Callback):
    def on_epoch_end(self, epoch, logs={}):
        yhat = self.model.predict_proba(x_valid, verbose=0)[:,1]
        print 'AUC', roc_auc_score(val_Y[:,1], yhat) 
        
def build_model():

    input_dim = train.shape[1] - 3
    classes = 2    
    
    model = Sequential()
    model.add(Dense(1024, input_dim= input_dim)) 
    model.add(Dropout(0.1))
    model.add(PReLU())
    model.add(BatchNormalization())
    model.add(Dropout(0.5))
    
    model.add(Dense(480))
    model.add(PReLU())
    model.add(BatchNormalization())
    model.add(Dropout(0.5))
    
    model.add(Dense(400))
    model.add(PReLU())
    model.add(BatchNormalization())
    model.add(Dropout(0.5))
    
    model.add(Dense(classes))
    model.add(Activation("softmax"))
    
    model.compile(loss='binary_crossentropy', optimizer="Adadelta")
    return model    
    
def build_model2():
    input_dim = train.shape[1] - 3
    classes = 2    
    
    model = Sequential()
    model.add(Dense(120, input_dim= input_dim)) 
    model.add(Dropout(0.1))
    model.add(Activation("relu"))
    model.add(Dropout(0.5))
    
    model.add(Dense(120))
    model.add(Activation("relu"))
    
    model.add(Dense(classes))
    model.add(Activation("softmax"))
    
    model.compile(loss = "binary_crossentropy", optimizer = "Adadelta")
    return model
    
def fit_model():
    
    feature_names = [f for f in train.columns if f not in ["QuoteNumber", "QuoteConversion_Flag", "train_flag"]]
    if need_validation:
        
        fold_ids = pd.read_csv("The Quants/validation_quotes.csv")
        
        nn_val = pd.DataFrame(columns = ["QuoteNumber", "nn_v1"])
        
        for i in xrange(4):
            
            print "\n--------------------------------------------"
            print "---------------- Fold %d --------------------" %i
            print "--------------------------------------------"            
            
            model = build_model()
            val_ids = fold_ids.ix[:, i].dropna()
            idx = train["QuoteNumber"].isin(list(val_ids))
            
            trainingSet = train[~idx]
            validationSet = train[idx]        
            
            tr_X = np.matrix(trainingSet[feature_names])
            tr_Y = np_utils.to_categorical(np.array(trainingSet["QuoteConversion_Flag"]))
            val_X = np.matrix(validationSet[feature_names])
            val_Y = np_utils.to_categorical(np.array(validationSet["QuoteConversion_Flag"]))
            model.fit(tr_X, tr_Y, validation_data=(val_X, val_Y), nb_epoch = nb_epoch, batch_size= batch_size)
            
            preds = model.predict_proba(val_X, batch_size=128)[:,1]
            df = pd.DataFrame({"QuoteNumber" : validationSet["QuoteNumber"], "nn_v1" : preds})
            nn_val = nn_val.append(df, ignore_index = True)
        
        return nn_val
    
    else:
        
        model = build_model()
        tr_X = np.matrix(train[feature_names])
        tr_Y = np_utils.to_categorical(np.array(train["QuoteConversion_Flag"]))
        test_X = np.matrix(test[feature_names])
        model.fit(tr_X, tr_Y, nb_epoch = nb_epoch, batch_size = batch_size)
        preds = model.predict_proba(test_X, batch_size = 128)[:, 1]
        test_df = pd.DataFrame({"QuoteNumber" : test["QuoteNumber"], "nn_v1" : preds})
        return test_df

if __name__ == '__main__':

    train, test = load_data()
    nn_val = fit_model()
    nn_val.to_csv("The Quants/Validation Predictions/bishwarup_nn_1_validation.csv", index = False)
    need_validation = False
    nn_test = fit_model()
    nn_test.to_csv("The Quants/Test Predictions/bishwarup_nn_1_test.csv", index = False)
