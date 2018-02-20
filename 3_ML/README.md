
### Summary of the code in src
The code in each folder does one step of hyper-parameter optimization and cross-valiation. The code for Artificial Neural Network is in Python and the code for SVM and Naive Bayes is in R.

### Folder structure
1. Folders related to ANN:
ANN_feature_selection: do backward feature selection using different hyper-parameters.
ANN_feature_selection3: change one of the hyper-parameters and see how the performance change
CV_ANN: cross validation using the selected features and optimal hyper-parameters.
CV_oversampling_ANN: cross validation with the rare class oversampled.
validation_ANN: do validation on the osmotic condition. 

2. Folders related to SVM_NB:
SVM_NB_feature_selection: do backward feature selection using different hyper-parameters (no hyper-parameters for NB)
CV_SVM_NB: cross validation using the selected features and optimal hyper-parameters.
CV_oversampling_SVM_NB: cross validation with  the rare class oversampled.
validation_SVM_NB: do validation on the osmotic condition.

### Dependencies
* R 3.2.3
* python3.6
* Tensorflow 1.2.0


### Data
The database for building the predictor is in ./out_data/MLDB.csv; The explored settings for the hyperparameters of ANN, SVM, and NB are in ./out_data/FFN_setting and ./out_data/SVM_NB_setting respectively.
Intermediate results are stored in the folder out_data.

### Running
In each folder of code in Python, there is script for submiting or running the job. The code in R can run in Rstudio after running the script named "Rproject.sh"/"Rproject.bat" to update the paths in the "setpath.R".
* Step1: Run the feature selection for each method:
  For ANN, run the code in ANN_feature_selection; For SVM and NB, run the code in SVM_NB_feature_selection. The code is run on Blue water. In order to submit each job run submit.pbs in that folders mentioned.

* Step2: Cross validation was conducted using selected features and optimal hyper-parameters.

### Support

If you have any questions about DeepPep, please contact Xiaokang wang (kanwang@ucdavis.edu).



```
```
```
``` 
