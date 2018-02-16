
import pandas as pd
import numpy as np
import keras
from keras.models import Sequential
from keras.layers import Dense, Dropout,BatchNormalization
from keras.optimizers import RMSprop
from keras.regularizers import l2,l1
from keras.optimizers import Adam,RMSprop,Adagrad

from sklearn.model_selection import LeaveOneOut
from sklearn.metrics import roc_curve, auc
import matplotlib.pyplot as plt
from keras.callbacks import EarlyStopping
from sklearn.metrics import average_precision_score

import collections
import sys
import  csv
import random

args = sys.argv
gene_index = int(args[1])
line_num = int(args[2])

# read in data
df = pd.read_csv("../../out_data/MLDB.csv")
first_gene_index = df.columns.get_loc("rrrD")
X, Y = np.split(df, [first_gene_index], axis=1)
X = X.values
X = X-0.5
Y = Y.values[:,gene_index]



def get_setting(file_path,line_num):
    #create a namedtuple
    digits = tuple([str(i) for i in range(9)])
    print("line_num:", line_num)
    with open(file_path) as fp:
        for i, line in enumerate(fp):
            if i == line_num:
                setting = [float(x)  if x.startswith(digits ) else x for x in line.strip().split(",")] 
                break
    print("setting: ",setting)
    return setting

Model_setting = collections.namedtuple('Model_setting','num_layers num_node alpha drop_rate act_method lr regularization patience opt_method')
setting_ = get_setting("../../out_data/FFN_setting",line_num)
setting = Model_setting(*setting_)
setting = setting._asdict()


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



callbacks = [EarlyStopping(monitor='acc',min_delta=0,patience = int(setting['patience']) )]


def cross_validation(X,Y,setting,num_input):
    model = getModel(setting,num_input)
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
    print("auc: ", roc_auc)
    return (roc_auc,average_precision)

def backward_selection(X,Y,setting):
    survive_index=[i for i in range(X.shape[1])]
    survive_index = random.sample(survive_index, 3)
    print("backward start survive_index", survive_index)
    best_ROCs = []
    best_PRs = []

    exclude_index = []
    for repeat_time  in range(len(survive_index)-1):
        print("excluded: ", exclude_index)
        print("best_perfs: ", best_PRs)

        best_perf = 0
        exclude_index_current = 0
        best_ROC = 0
        best_PR = 0
        for index in survive_index:
            survive_index_copy = [i for i in survive_index if i!=index]
            perf = cross_validation(X[:,survive_index_copy],Y,setting,num_input = len(survive_index)-1)
            print("perf: inner loop", perf)
            if perf[0]+perf[1] > best_perf:
               best_perf = perf[0] + perf[1]
               best_ROC = perf[0]
               best_PR = perf[1]
               exclude_index_current = index
        if repeat_time==0 or  best_perf > best_ROCs[-1] + best_PRs[-1]:
            best_ROCs.append(best_ROC)
            best_PRs.append(best_PR)
            exclude_index.append(exclude_index_current)
        else:
            break
    return (exclude_index,best_ROCs,best_PRs)

res = backward_selection(X,Y,setting)
print("res",res)



res_path = "../../out_data/feature_selection/ANN/"+"gene_"+str(gene_index)+"_setting_"+str(line_num)
with open(res_path,"w") as f:
    wr = csv.writer(f)
    wr.writerows(res)

print("end sucessfully")
