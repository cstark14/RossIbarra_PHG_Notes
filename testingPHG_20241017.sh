module load conda
module load jdk
conda activate PHG
export PATH="/group/jrigrp11/cstark/phg/bin:$PATH"

### post phg environment creation
conda activate phgv2-conda

### kmer read mapping
phg map-kmers \
    --hvcf-dir /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/vcf_dbs/hvcf_files \
    --kmer-index /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files/kmerIndex.txt \
    --key-file /group/jrigrp11/cstark/synDHfastqPHGmapped/ \
    --output-dir /group/jrigrp11/cstark/synDHfastqPHGmapped/

### GBS reads here: /group/jrigrp10/synthetic_load/fastq_data/synDH_fastq

#### keyfile is this structure. can support single reads (just filename not filename2)
#sampleName	filename	filename2
#sample1	read1.fq	read2.fq
#sample2	read3.fq	read4.fq
#sample3	read5.fq