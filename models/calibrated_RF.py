# -*- coding: utf-8 -*-
"""
Created on Sun Nov 15 23:08:46 2015

@author: bishwarup
"""
import os
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn.calibration import CalibratedClassifierCV

os.chdir("/home/bishwarup/Kaggle/Homesite")

md = pd.read_csv("Data/homesite_data.csv")

fltr = ["_ev" not in x for x in md.columns]
md2 = md.loc[:,fltr]

ptr = md[md.split1 == 0]
pte = md[md.split1 == 2]

target = ptr["QuoteConversion_Flag"]

ptr.drop(["QuoteNumber", "Original_Quote_Date", "QuoteConversion_Flag", "split1"], axis = 1, inplace = True)
pte.drop(["QuoteNumber", "Original_Quote_Date", "QuoteConversion_Flag", "split1"], axis = 1, inplace = True)

clf = RandomForestClassifier(n_estimators = 2000,
                             criterion = "gini",
                             max_features = 0.6,
                             max_depth = 10,
                             min_samples_split = 6,
                             n_jobs = -1,
                             random_state = 29,
                             verbose = 1,
                             class_weight = "auto")

clf_isotonic = CalibratedClassifierCV(clf, cv = 4, method = "isotonic")

clf_isotonic.fit(ptr, target)
prob_pos_isotonic = clf_isotonic.predict_proba(pte)[:, 1]
sub = pd.DataFrame(prob_pos_isotonic)
sub.columns = ["QuoteConversion_Flag"]
sub.to_csv("calibratedRF.csv", index = False)