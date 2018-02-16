cd ../../out_data/validation/
for i  in `seq 0 25 1990`

do 
tail -n +2 ANN_val${i}.csv |head -n25>> ANN_val.csv

done

