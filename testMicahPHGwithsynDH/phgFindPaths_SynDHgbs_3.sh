#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/filesFromMicahK_Danforth/
#SBATCH -J phgReadMap_SynDHgbs_3
#SBATCH -o /group/jrigrp11/cstark/filesFromMicahK_Danforth/out-%A.txt
#SBATCH -e /group/jrigrp11/cstark/filesFromMicahK_Danforth/error-%A.txt
#SBATCH -t 24:00:00
#SBATCH --partition=high2
#SBATCH --mem=122880

module load conda
module load jdk

export PATH="/group/jrigrp11/cstark/phg/bin:$PATH"
export JAVA_OPTS="-Xmx100g"

phg find-paths \
    --path-keyfile /group/jrigrp11/cstark/readMappedSynDHgbs_keyfile.txt \
    --hvcf-dir /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files \
    --reference-genome /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/updated_assemblies/B73.fa \
    --path-type haploid \
    --output-dir /group/jrigrp11/cstark/synDH_vcfFilesImputedProbLikely0.5 \
    --prob-correct=0.50 \
    --out-parents-dir /group/jrigrp11/cstark/synDH_likelyAncestorsProbLikely0.5 > /group/jrigrp11/cstark/synDH_likelyAncestorsProbLikely0.5/pathFinding3_prob0.50_20241023.out 2> /group/jrigrp11/cstark/synDH_likelyAncestorsProbLikely0.5/pathFinding3_prob0.50_20241023.err