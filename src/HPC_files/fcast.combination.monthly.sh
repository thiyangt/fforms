#!/bin/env bash
#SBATCH --job-name=M4M
#SBATCH --time=100:00:00
#SBATCH --mem=100G
#SBATCH --ntasks=1
#SBATCH --array=0-19
#SBATCH --partition=short,gpu,comp
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=m4mcombination-%a.txt
#SBATCH --open-mode=append
module load R/3.5.1

echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID
Rscript fcast.combination.monthly.R $SLURM_ARRAY_TASK_ID