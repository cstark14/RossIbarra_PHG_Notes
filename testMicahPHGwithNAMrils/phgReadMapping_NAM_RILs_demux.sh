#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/namRIL_GBS/
#SBATCH -J phgNamRILSmapping
#SBATCH -o /group/jrigrp11/cstark/namRIL_GBS/logs/%x_%j.sbatch.out
#SBATCH -e /group/jrigrp11/cstark/namRIL_GBS/logs/%x_%j.sbatch.err
#SBATCH -t 24:00:00
#SBATCH --partition=high2
#SBATCH --ntasks=30
#SBATCH --mem=240000

module load conda
module load jdk

export PATH="/group/jrigrp11/cstark/phg/bin:$PATH"
export JAVA_OPTS="-Xmx240g"

phg map-kmers \
    --hvcf-dir /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files \
    --key-file /group/jrigrp11/cstark/RossIbarra_PHG_Notes/testMicahPHGwithNAMrils/NAM_RILs_SRP009896_demux_mappingKeyfile.txt \
    --output-dir /group/jrigrp11/cstark/namRIL_GBS/readmapped_demux/