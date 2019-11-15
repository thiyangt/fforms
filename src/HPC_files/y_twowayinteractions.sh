#!/bin/env bash
#SBATCH --job-name=y_interactions
#SBATCH --time=10:00:00
#SBATCH --mem=70G
#SBATCH --ntasks=1
#SBATCH --partition=short
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=y_twowayinteractions.txt
module load R/3.5.1
R --vanilla < y_twowayinteractions.R > y_twowayinteractions.txt