#!/bin/bash

# Submit the pipeline as a job with qsub job.sh

# Modified from https://github.com/mschubert/clustermq/blob/master/inst/LSF.tmpl
# under the Apache 2.0 license:
#PBS -N targets
#PBS -l nodes=1:ppn=1:mem=4096MB
#PBS -l walltime=24:00:00 # extend for longer pipeline runtimes
#PBS -o /dev/null
#PBS -j oe

R CMD BATCH run.R

# Removing .RData is recommended.
# rm -f .RData
