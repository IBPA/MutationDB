
import pandas as pd
import numpy as np
import keras
from keras.models import Sequential,Model
from keras.layers import Dense, Dropout,BatchNormalization,Input
from keras.optimizers import RMSprop
from keras.regularizers import l2,l1
from keras.optimizers import Adam

import os
import collections


df = pd.read_csv("../../out_data/MLDB_medium.csv")
setting = pd.read_csv("../../out_data/CultureCondition.csv", dtype={'Temperature':object})

setting = ["_".join(pair) for pair in zip(setting.columns,setting.iloc[0,:])]
mask = [True if name in setting else False for name in df.columns[:79] ]


first_gene_index = df.columns.get_loc("rrrD")

X, Y = np.split(df, [first_gene_index], axis=1)
X = X.values
X = X-0.5



Model_setting = collections.namedtuple('Model_setting','num_layers num_node alpha drop_rate act_method lr regularization patience')



setting_ = [2,100, 0.5, 0.2, 'tanh', 0.01, 'l2', 3]
setting = Model_setting(*setting_)
setting = setting._asdict()


num_output_ = 1
def create_model(num_input = 79,num_output = num_output_):
    X_input = Input(shape=(num_input,))

    X = Dense(64)(X_input)
    X = Dropout(0.2)(X)
    X = Dense(32)(X)
    Ys= []
    for i in range(num_output):
        Ys.append(Dense(1, activation = 'sigmoid')(X))
    model = Model(inputs=[X_input],outputs = Ys)
    model.compile(loss=['binary_crossentropy']*num_output,loss_weights=[1.]*num_output,optimizer=Adam(lr=setting['lr']), metrics=['accuracy'])
    return model


model = create_model()


preds = []
test = X[0:1,:]
test[0:1,:] = -0.5
test[0:1,mask] = 0.5
for i in range(1990):
    model_path = "../../out_model/ANN%s.h5"%(str(i))
    model.load_weights(model_path)

    pred  = model.predict(test)
    pred = np.squeeze(np.array(pred))
    preds.append(pred)
preds = np.array(preds)


res = pd.DataFrame()
res["genome site"] = df.columns[80:]
res["probability"] = preds


res.to_csv("../../out_pred/ANN.pred.csv", index = False)
