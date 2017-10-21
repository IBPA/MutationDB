#!/bin/bash

models=(NB ANN)

for model in "${models[@]}" 
do
	for i in {1..1992}
	do
	   sed -i "s/models.R.*/models.R $i $model/g" one_job.sh
	   sbatch one_job.sh

	done
done
