srun -p bigmemh -t 2:00:00 --pty bash

module load conda
module load jdk
conda activate PHG
export PATH="/group/jrigrp11/cstark/phg/bin:$PATH"
export JAVA_OPTS="-Xmx100g"

### post phg environment creation
conda activate phgv2-conda

### kmer read mapping
## ignoring micah's info
phg map-kmers \
    --hvcf-dir /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files \
    --kmer-index /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files/kmerIndex.txt \
    --key-file /group/jrigrp11/cstark/synDHfastqPHGmapped/ \
    --output-dir /group/jrigrp11/cstark/synDHfastqPHGmapped/

#### according to Micah: In the imputation workflows they use the set in phg_v2.4.8.162_ZeaSyn/output/vcf_files. kmer index command wouldnt be needed
phg map-kmers \
    --hvcf-dir /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files \
    --key-file /group/jrigrp11/cstark/synDHfastqPHGmapped/ \
    --output-dir /group/jrigrp11/cstark/synDHfastqPHGmapped/

phg map-kmers \
    --hvcf-dir /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files \
    --key-file /group/jrigrp11/cstark/synDHfastqPHGmapped/fastqList1_phgMappingKeyFile_oneSample.txt \
    --output-dir /group/jrigrp11/cstark/synDHfastqPHGmapped/ \
    --kmer-index /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files/kmerIndex.txt \
    --diagnostic-mode > mappingSingleDHgbs_20241019.out
### did not work. again killed even after setting java mem to 100 gbs,
### diagnostic mnode did nothing. cool

### trying with phg github examples
phg map-kmers \
    --hvcf-dir  \
    --key-file  \
    --output-dir /group/jrigrp11/cstark// \
    --kmer-index  \
    --diagnostic-mode > .out


### GBS reads here: /group/jrigrp10/synthetic_load/fastq_data/synDH_fastq

#### keyfile is this structure. can support single reads (just filename not filename2)
#sampleName	filename	filename2
#sample1	read1.fq	read2.fq
#sample2	read3.fq	read4.fq
#sample3	read5.fq