#!/bin/bash

# You can run this script in a shell either with ./run.sh or srun run.sh

# Modified from https://github.com/mschubert/clustermq/blob/master/inst/LSF.tmpl
# under the Apache 2.0 license:
#SBATCH --job-name=targets
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null
#SBATCH --mem-per-cpu=4096
#SBATCH --cpus-per-task=1

# This script runs the pipeline in a persistent background process:
nohup nice -4 R CMD BATCH run.R &

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
# rm -f .RData
