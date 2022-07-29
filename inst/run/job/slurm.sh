#!/bin/bash

# Submit the pipeline as a job with srun job.sh

# Modified from https://github.com/mschubert/clustermq/blob/master/inst/LSF.tmpl
# under the Apache 2.0 license:
#SBATCH --job-name=JOB_NAME
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null
#SBATCH --mem-per-cpu=4096
#SBATCH --cpus-per-task=1

module load R # Comment out if R is not an environment module.
R CMD BATCH run.R

# Removing .RData is recommended.
# rm -f .RData
