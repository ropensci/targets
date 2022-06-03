#!/bin/bash

# You can run this script in a shell either with ./run.sh or bsub run.sh

# Modified from https://github.com/mschubert/clustermq/blob/master/inst/LSF.tmpl
# under the Apache 2.0 license:
#BSUB-J targets
#BSUB-n 1
#BSUB-o /dev/null
#BSUB-M 4096
#BSUB-R rusage[mem=4096]
#BSUB-R span[ptile=1]

# This script runs the pipeline in a persistent background process:
nohup nice -4 R CMD BATCH run.R &

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
# rm -f .RData
