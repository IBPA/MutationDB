#!/bin/bash
hypers=(relu adagrad RMSProp Sigmoid)
for ((i=0; i<${#hypers[@]};i++))
do 
	python change_one_hyper_parameter.py ${hypers[$i]}

done
