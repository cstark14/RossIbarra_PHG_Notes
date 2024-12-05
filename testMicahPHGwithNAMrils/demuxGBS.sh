#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/
#SBATCH -J demuxGBS
#SBATCH -o /group/jrigrp11/cstark/namRIL_GBS/logs/%x_%j.sbatch.out
#SBATCH -e /group/jrigrp11/cstark/namRIL_GBS/logs/%x_%j.sbatch.err
#SBATCH -t 24:00:00
#SBATCH --partition=high2
#SBATCH --ntasks=30
#SBATCH --mem=240000

pip install demultiplex

export PATH="/home/vale878/.local/bin/:$PATH"

#demultiplex demux -r --format unknown /group/jrigrp11/cstark/RossIbarra_PHG_Notes/testMicahPHGwithNAMrils/sampleNamesBarcodes.txt /group/jrigrp11/cstark/RossIbarra_PHG_Notes/testMicahPHGwithNAMrils/rawReads/*
demultiplex demux -r -m5 -d -p /group/jrigrp11/cstark/namRIL_GBS/rawReads_demuxByName/. /group/jrigrp11/cstark/RossIbarra_PHG_Notes/testMicahPHGwithNAMrils/sampleNamesBarcodes.txt \
    /group/jrigrp11/cstark/namRIL_GBS/rawReads/SRR391097.fastq.gz 
