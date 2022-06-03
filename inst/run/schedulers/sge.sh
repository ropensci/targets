#!/bin/bash

# You can run this script in a shell either with ./run.sh or qsub run.sh

# Modified from https://github.com/mschubert/clustermq/blob/master/inst/LSF.tmpl
# under the Apache 2.0 license:
#$ -N targets                    # Name of the job. You can add custom prefixes.
#$ -j y                          # Merge stdout and stderr into one stream for the log files.
#$ -o /dev/null                  # Log file or directory of log files.
#$ -cwd                          # Working directory agrees with that of the launching process.
#$ -V                            # Use environment variables.

# This script runs the pipeline in a persistent background process:
nohup nice -4 R CMD BATCH run.R &

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
# rm -f .RData
