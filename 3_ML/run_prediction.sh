./setup_dir.sh
cd ./src/predictions

Rscript NB_pred.R
Rscript SVM_pred.R
python ANN.py
Rscript merge_pred.R
