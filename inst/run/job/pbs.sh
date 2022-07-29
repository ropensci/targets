#!/bin/bash

# Submit the pipeline as a job with qsub job.sh

# Modified from https://github.com/mschubert/clustermq/blob/master/inst/LSF.tmpl
# under the Apache 2.0 license:
#PBS -N JOB_NAME
#PBS -l nodes=1:ppn=1:mem=4096MB
#PBS -l walltime=24:00:00 # extend for longer pipeline runtimes
#PBS -o /dev/null
#PBS -j oe

module load R # Comment out if R is not an environment module.
R CMD BATCH run.R

# Removing .RData is recommended.
# rm -f .RData
