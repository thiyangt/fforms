#!/bin/env bash
#SBATCH --job-name=m_interactions
#SBATCH --time=10:00:00
#SBATCH --mem=60G
#SBATCH --ntasks=1
#SBATCH --partition=short
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=thiyanga.talagala@monash.edu
#SBATCH --output=m_interactions.txt
module load R/3.5.1
R --vanilla < m_twowayinteractions.R > m_twowayinteractions.txt