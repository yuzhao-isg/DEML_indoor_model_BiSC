#!/bin/bash

#SBATCH --job-name=DEML # Set a Job name
#SBATCH --partition=long # Set a partition(time) or queue to submit to
#SBATCH --mail-type=BEGIN,END,FAIL # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=yu.zhao@isglobal.org # Where to send mail events
#SBATCH --ntasks=1 # Run a single tasks
#SBATCH --cpus-per-task=8 # Use 4 cpus for each task
#SBATCH --mem=12gb # Job memory request
#SBATCH --output=/PROJECTES/BISC_DATA/analyses/BiSC_24/010_Yu_Zhao/results/log/test_%j.log # Standard output and error log
# Clear the environment from any previously loaded modules
module purge > /dev/null 2>&1

# Load the module environment suitable for the job. In this case, R version 4.2.2
source ~/.bashrc
conda activate indoor_model_env

# And finally run the job calling R as you wold do using system R installation
R CMD BATCH $PROJECT_ROOT/script/indoor_temp/DEML/Simple_Model/Temporal_longterm_validation/10_DEML_weekly_BE.R