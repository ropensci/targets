#!/bin/bash

# Submit the pipeline as a background process with ./run.sh
nohup nice -4 R CMD BATCH run.R &

# Change the nice level above as appropriate
# for your situation and system.

# Removing .RData is recommended.
# rm -f .RData
