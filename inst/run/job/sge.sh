#!/bin/bash

# Submit the pipeline as a job with qsub job.sh

# Modified from https://github.com/mschubert/clustermq/blob/master/inst/LSF.tmpl
# under the Apache 2.0 license:
#$ -N targets                    # Name of the job.
#$ -j y                          # Merge stdout and stderr into one stream for the log files.
#$ -o /dev/null                  # Log file or directory of log files.
#$ -cwd                          # Working directory agrees with that of the launching process.
#$ -V                            # Use environment variables.

# module load R # Uncomment if R is an environment module.
R CMD BATCH run.R

# Removing .RData is recommended.
# rm -f .RData
