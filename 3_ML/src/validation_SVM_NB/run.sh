#!/bin/bash
for i in `seq 1 1 10`
do
	Rscript NB_performance.R $i
	Rscript SVM_performance.R $i
done
