#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/
#SBATCH -J phgNamRILSimpute0.90_recomb0.000001_maxAncestor2
#SBATCH -o /group/jrigrp11/cstark/namRIL_GBS/%x_%j.sbatch.out
#SBATCH -e /group/jrigrp11/cstark/namRIL_GBS/%x_%j.sbatch.err
#SBATCH -t 24:00:00
#SBATCH --partition=high2
#SBATCH --ntasks=30
#SBATCH --mem=240000

module load conda
module load jdk

export PATH="/group/jrigrp11/cstark/phg/bin:$PATH"
export JAVA_OPTS="-Xmx240g"

phg find-paths \
    --path-keyfile /group/jrigrp11/cstark/RossIbarra_PHG_Notes/testMicahPHGwithNAMrils/NAM_RILs_SRP009896_imputeKeyfile.txt \
    --hvcf-dir /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files \
    --reference-genome /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/updated_assemblies/B73.fa.gz \
    --path-type haploid \
    --output-dir /group/jrigrp11/cstark/namRIL_GBS/imputed_prob0.90_recomb0.0000001_maxAncestor2 \
    --prob-correct 0.90 \
    --prob-same-gamete 0.999999 \
    --use-likely-ancestors true \
    --max-ancestors 2 \
    --likely-ancestor-file /group/jrigrp11/cstark/namRIL_GBS/phgNamRILSimpute_Acc0.90_recomb0.0000001_maxAncestor2_likelyAncestors.txt \
    --out-parents-dir /group/jrigrp11/cstark/namRIL_GBS/imputed_prob0.90_recomb0.0000001_maxAncestor2/parents #> /group/jrigrp11/cstark/namRIL_GBS/phgNamRILSimpute_Acc0.90_CML277_20241203.out 2> /group/jrigrp11/cstark/namRIL_GBS/phgNamRILSimpute_Acc0.90_CML277_20241203.err