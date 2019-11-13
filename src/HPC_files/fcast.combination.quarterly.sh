#!/bin/env bash
#SBATCH --job-name=fccQuarterly
#SBATCH --time=100:00:00
#SBATCH --mem=60G
#SBATCH --ntasks=1
#SBATCH --partition=short,gpu,comp
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=fcast.combination.quarterly.txt
module load R/3.5.1
R --vanilla < fcast.combination.quarterly.R > fcast.combination.quarterly.txt