#!/bin/env bash
#SBATCH --job-name=wfforma
#SBATCH --time=90:00:00
#SBATCH --mem=50G
#SBATCH --ntasks=1
#SBATCH --partition=short,gpu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=computing_time_weekly_fforma.txt
module load R/3.5.1
module load cuda/7.5
R --vanilla < computing_time_weekly_fforma.R > computing_time_weekly_fforma.txt