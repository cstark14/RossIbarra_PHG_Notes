#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/namRIL_GBS/
#SBATCH -J gzipNAM_RILs_GBS
#SBATCH -o /group/jrigrp11/cstark/namRIL_GBS/out-%A_gzip.txt
#SBATCH -e /group/jrigrp11/cstark/namRIL_GBS/error-%A_gzip.txt
#SBATCH -t 24:00:00
#SBATCH --partition=high2

module load sratoolkit

gzip /group/jrigrp11/cstark/namRIL_GBS/rawReads/*.fastq