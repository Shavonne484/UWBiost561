#!/bin/bash

#SBATCH --job-name=demo
#SBATCH --account=biostat
#SBATCH --partition=all-12c128g

#SBATCH --time=12:00:00
#SBATCH --mem-per-cpu=10gb

R CMD BATCH --no-save --no-restore demo_bayes.R
