

import sys
import pandas as pd
import numpy as np
import keras
from keras.models import Sequential
from keras.layers import Dense, Dropout,BatchNormalization
from keras.optimizers import RMSprop,Adagrad
from keras.regularizers import l2,l1
from keras.optimizers import Adam,RMSprop

from sklearn.model_selection import LeaveOneOut
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt
from keras.callbacks import EarlyStopping

from sklearn.metrics import average_precision_score
import collections
import random


df = pd.read_csv("../../out_data/MLDB.csv",encoding = "latin-1")

first_gene_index = df.columns.get_loc("rrrD")



def get_feature_index(file_path,line_num):

    with open(file_path) as fp:
        for i, line in enumerate(fp):
            if i == line_num:
                removed = [int(x) for x in line.strip().split(" ")]
                break
    indexes = [i for i in range(84) if  i not in removed[:-1]]
    setting_index = removed[-1]
    return (indexes,setting_index)




def get_setting(file_path,line_num):
    digits = tuple([str(i) for i in range(9)])
    with open(file_path) as fp:
        for i, line in enumerate(fp):
            if i == line_num:
                setting = [float(x)  if x.startswith(digits ) else x for x in line.strip().split(",")] 
                break
    return setting



def getModel(setting,num_input=84):
    regularizer = l1(setting['alpha']) if setting['regularization']=='l1' else l2(setting['alpha'])
    optimizers = {"adam":  Adam(lr=setting['lr']), "RMSProp":RMSprop(lr=setting['lr']), "Adagrad": Adagrad(lr=0.01) }

    optimizer = optimizers[setting['opt_method']]
    num_nodes = [int(i)  for i in setting['num_node'].split("_") if i!=""]

    model = Sequential()
    for i in range(int(setting['num_layers'])):
        if i==0:
            model.add(Dense( num_nodes[i], input_shape=(num_input,), activation=setting['act_method'],                            kernel_regularizer = regularizer))
            model.add(Dropout(setting['drop_rate']))
        else:
            model.add(Dense( num_nodes[i], activation=setting['act_method']))
            model.add(Dropout(setting['drop_rate']))
    model.add(Dense(1, activation='sigmoid'))
    model.compile(loss='binary_crossentropy', optimizer=Adam(lr=setting['lr']), metrics=['accuracy'])
    return model



callbacks = [EarlyStopping(monitor='acc',min_delta=0,patience=setting['patience'])]


def cross_validation(X,Y,setting):
    model = getModel(setting,X.shape[1])
    preds = []
    for train, test in LeaveOneOut().split(X, Y):
        model.fit(X[train,:],Y[train],epochs=20,verbose=0, callbacks =callbacks)
        probas_ = model.predict(X[test,:])
        preds.append(probas_[0][0])
        # Compute ROC curve and area the curve
    fpr, tpr, thresholds = roc_curve(Y, preds)
    average_precision = average_precision_score(Y, preds)
    roc_auc = auc(fpr, tpr)
    if roc_auc < 0.5:
        roc_auc = 1 - roc_auc
    
    return preds



Xs, Ys = np.split(df, [first_gene_index], axis=1)
Xs = Xs.values
Xs = Xs-0.5

for gene_id in range(0,1990):
    Y = Ys.values[:,gene_id]
    best_index_setting = "../../out_data/feature_selection/selected_features/MLDB_ANN.csv"
    indexes, setting_index = get_feature_index(best_index_setting,gene_id+1)# the first line is meta information
    X = Xs[:,indexes]
    Model_setting = collections.namedtuple('Model_setting','num_layers num_node alpha drop_rate act_method lr regularization patience opt_method')


    setting_ = get_setting("../../out_data/FFN_setting2",setting_index )
    setting = Model_setting(*setting_)
    setting = setting._asdict()
    res = cross_validation(X,Y,setting)
    res = pd.DataFrame(res)

    res.columns =["pred"]
    out_path = "../../out_data/ANN_prediction/gene_id_"+str(gene_id) +".csv"
    res.to_csv(out_path,index = False)


