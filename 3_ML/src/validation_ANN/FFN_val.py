import pandas as pd
import numpy as np
import keras
from keras.models import Sequential
from keras.layers import Dense, Dropout,BatchNormalization
from keras.optimizers import RMSprop,Adagrad,Adam
from keras.regularizers import l2,l1


from sklearn.model_selection import LeaveOneOut
from sklearn.metrics import roc_curve, auc

from keras.callbacks import EarlyStopping

from sklearn.metrics import average_precision_score
import collections



df = pd.read_csv("../../out_data/MLDB.csv")
first_gene_index = df.columns.get_loc("rrrD")
best_settings = pd.read_csv("../../out_data/feature_selection/ANN_perf_feature_selection.csv")



exp_mut = pd.read_csv("../../data/Mutation_freq_product.csv")

from sklearn.utils import shuffle
X, Ys = np.split(df, [first_gene_index], axis=1)
X = X.values
X = X-0.5


# exp_mut.iloc[:,0]

# In[29]:

exp_binary = []
for gene in Ys.columns:
    if gene in list(exp_mut.iloc[:,0]):
        exp_binary.append(1)
    else:
        exp_binary.append(0)


# In[30]:

truth = pd.DataFrame(exp_binary[:1990])
truth.columns = ["truth"]
truth.to_csv("../../data/val_truth.csv",index = False)


# In[31]:

indexes = []
hit_genes = []
truth = []
for gene in exp_mut.iloc[:,0]:
    if gene in Ys.columns:
        hit_genes.append(gene)
        indexes.append(Ys.columns.get_loc(gene))
        truth.append(1)


# In[32]:

Model_setting = collections.namedtuple('Model_setting','num_layers num_node alpha drop_rate act_method lr regularization patience opt_method')


# In[33]:

def get_setting(file_path,line_num):
    digits = tuple([str(i) for i in range(9)])
    with open(file_path) as fp:
        for i, line in enumerate(fp):
            if i == line_num:
                setting = [float(x)  if x.startswith(digits ) else x for x in line.strip().split(",")] 
                break
    return setting



# In[34]:


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


# callbacks = [EarlyStopping(monitor='acc',min_delta=0,patience=setting['patience'])]

# In[41]:

import sys
args = sys.argv
start = int(args[1])
repeat = args[2]

end = start+25
preds = []


# In[44]:

end = min(end, 1990)
for gene_id in range(start,end,1):
    Y = Ys.values[:,gene_id]

    # gettting line_num for the best setting
    line_num = int(best_settings.ix[gene_id,]['best_files'].split("_")[-1])
    setting_ = get_setting("../../out_data/FFN_setting2",0)
    setting = Model_setting(*setting_)
    setting = setting._asdict()

    # use the selected features only
    removed_features = pd.read_csv("../../out_data/feature_selection/ANN2/"                                   +best_settings.ix[1,'best_files'],header = None)
    removed_index = set([int(i) for i in removed_features.ix[0,]])
    survive_index =[i for i in range(84) if not i in removed_index]


    removed_features = pd.read_csv("../../out_data/feature_selection/ANN2/"                                   +best_settings.ix[gene_id,]['best_files'],header=None)

    model = getModel(setting,len(survive_index))
    index = np.random.choice(range(X.shape[0]), X.shape[0]*2)

    train = X[index,:]
    train = train[:,survive_index]

    Y_train = Y[index]

    print("Ys shape", Ys.shape)

    print("train shape", train.shape)
    model.fit(train,Y_train,epochs=20,verbose=0)

    preds.append(model.predict(X[1:2,survive_index])[0][0])
    if gene_id%25 ==0:
        print(gene_id)


# In[119]:

out = pd.DataFrame(preds)
out.columns = ["pred"]


# In[124]:

out.to_csv("../../out_data/validation/ANN_validation/str(repeat)+"_"+str(start)+"_.csv",index = False)

