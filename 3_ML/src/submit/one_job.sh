#!/bin/bash
#SBATCH -p med
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --mem-per-cpu 2000
#SBATCH -t 10:00:00
#SBATCH -o log/slurm.%N.%j.out
#SBATCH -e log/slurm.%N.%j.err

cd ../client
srun -n 1 Rscript ./feature_selection_3_models.R 1992 ANN

