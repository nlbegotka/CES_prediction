#!/bin/bash
#SBATCH --partition=day
#SBATCH --job-name=RF_HEAT_STATE
#SBATCH --ntasks=1 --nodes=1
#SBATCH --cpus-per-task=12
#SBATCH --mem-per-cpu=5G
#SBATCH --time=12:00:00
#SBATCH --mail-type=ALL

# Run R code
module purge
module load R/4.3.0-foss-2020b
# module list -- lists all loaded modules 
export OMP_NUM_THREADS=1
Rscript rf_heat_state_hpc.R

