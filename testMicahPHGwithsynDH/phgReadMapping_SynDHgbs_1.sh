#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/filesFromMicahK_Danforth/
#SBATCH -J phgReadMap_SynDHgbs_1
#SBATCH -o /group/jrigrp11/cstark/filesFromMicahK_Danforth/out-%A_gzip.txt
#SBATCH -e /group/jrigrp11/cstark/filesFromMicahK_Danforth/error-%A_gzip.txt
#SBATCH -t 24:00:00
#SBATCH --partition=high2
#SBATCH --mem=122880

module load conda
module load jdk

export PATH="/group/jrigrp11/cstark/phg/bin:$PATH"
export JAVA_OPTS="-Xmx100g"

phg map-kmers \
    --hvcf-dir /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files \
    --key-file /group/jrigrp11/cstark/synDHfastqPHGmapped/fastqList1_phgMappingKeyFile.txt \
    --output-dir /group/jrigrp11/cstark/synDHfastqPHGmapped/ > /group/jrigrp11/cstark/mappingfastqList1_20241021.out 2> /group/jrigrp11/cstark/mappingfastqList1_20241021.err