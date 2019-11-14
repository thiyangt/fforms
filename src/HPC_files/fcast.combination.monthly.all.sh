#!/bin/env bash
#SBATCH --job-name=fccMonthly
#SBATCH --time=100:00:00
#SBATCH --mem=100G
#SBATCH --ntasks=1
#SBATCH --partition=short,gpu,comp
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=fcast.combination.monthly.all.txt
module load R/3.5.1
R --vanilla < fcast.combination.monthly.all.R > fcast.combination.monthly.all.txt