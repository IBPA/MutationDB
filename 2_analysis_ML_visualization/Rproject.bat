set homepath=%cd%

cd %homepath%
echo Project files structure > Files_structure.txt
md data
md src
md out_fig
md out_data
md ref


start %homepath%

cd src


md preprocess_data
md helper
md client



echo  data_path=^"%homepath:\=/%/data^" > setpath.R
echo  ref_path=^"%homepath:\=/%/ref^" >> setpath.R
echo  out_data_path=^"%homepath:\=/%/out_data^" >> setpath.R
echo  out_fig_path=^"%homepath:\=/%/out_fig^" >> setpath.R

echo  out_model_path=^"%homepath:\=/%/out_model^" >> setpath.R



echo  src_path=^"%homepath:\=/%/src^" >> setpath.R
echo preprocess_path = ^"%homepath:\=/%/src/preprocess_data^" >> setpath.R
echo helper_path =  ^"%homepath:\=/%/src/helper^" >> setpath.R

echo plot_path =  ^"%homepath:\=/%/src/client^" >> setpath.R


echo source(^"setpath.R^") > 1_.R

echo setwd(data_path)>>1_.R

echo setwd(module_1_path)>>1_.R

echo setwd(src_path)>>1_.R
echo setwd(out_data_path)>>1_.R

echo setwd(out_fig_path)>>1_.R


