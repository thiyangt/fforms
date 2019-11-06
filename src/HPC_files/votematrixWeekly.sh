#!/bin/env bash
#SBATCH --job-name=weeklyVote
#SBATCH --time=10:00:00
#SBATCH --mem=60G
#SBATCH --ntasks=1
#SBATCH --partition=short,gpu,comp
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=votematrixWeekly.txt
module load R/3.5.1
R --vanilla < votematrixWeekly.R > votematrixWeekly.txt