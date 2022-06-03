#!/bin/bash

# Submit the pipeline as a job with bsub job.sh

# Modified from https://github.com/mschubert/clustermq/blob/master/inst/LSF.tmpl
# under the Apache 2.0 license:
#BSUB-J targets
#BSUB-n 1
#BSUB-o /dev/null
#BSUB-M 4096
#BSUB-R rusage[mem=4096]
#BSUB-R span[ptile=1]

R CMD BATCH run.R

# Removing .RData is recommended.
# rm -f .RData
