homepath=$PWD

echo "ref_path=\"$homepath/ref\"" > ./src/setpath.R

echo  "data_path=\"$homepath/data\"" >> ./src/setpath.R
echo  "out_data_path=\"$homepath/out_data\"" >> ./src/setpath.R
echo  "out_pred_path=\"$homepath/out_pred\"" >> ./src/setpath.R
echo  "out_fig_path=\"$homepath/out_fig\"" >> ./src/setpath.R
echo  "out_model_path=\"$homepath/out_model\"" >> ./src/setpath.R

echo  "src_path=\"$homepath/src\"" >> ./src/setpath.R
echo "model_path=\"$homepath/src/model\"" >> ./src/setpath.R
echo "helper_path= \"$homepath/src/helper\"" >> ./src/setpath.R

echo "plot_path = \"$homepath/src/client\"" >> ./src/setpath.R

