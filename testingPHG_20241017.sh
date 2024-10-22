srun -p high2 -t 2:00:00 --mem 122880 --pty bash

module load conda
module load jdk

#### conda config --set solver libmamba

### post phg environment creation
#might not have to do the activate conda step
#conda activate phgv2-conda
export PATH="/group/jrigrp11/cstark/phg/bin:$PATH"
export JAVA_OPTS="-Xmx100g"

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
    --diagnostic-mode > mappingSingleDHgbs_20241021.out
### did not work. again killed even after setting java mem to 100 gbs,
### diagnostic mnode did nothing. cool
### trying again without line breaks
phg map-kmers --hvcf-dir /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files --key-file /group/jrigrp11/cstark/synDHfastqPHGmapped/fastqList1_phgMappingKeyFile_oneSample.txt --output-dir /group/jrigrp11/cstark/synDHfastqPHGmapped/ --diagnostic-mode

### micah's code
phg map-kmers \
    --hvcf-dir phg_v2.4.8.162_ZeaSyn/output/vcf_files \
    --key-file read_mapping_data.txt \
    --output-dir phg_v2.4.8.162_ZeaSyn/output/read_mappings_all_default \
    --threads 16 \
    --diagnostic-mode

### micah's code sub out keyfile
phg map-kmers \
    --hvcf-dir phg_v2.4.8.162_ZeaSyn/output/vcf_files \
    --key-file ./phgMappingKeyFile_oneSampleNotGZipped.txt \
    --output-dir phg_v2.4.8.162_ZeaSyn/output/read_mappings_all_default \
    --threads 16 \
    --diagnostic-mode
### still doesnt work
#### IT WAS STUPID BIGMEM ISSUE THE WHOLE TIME

###trying to rebuild kmer index
phg build-kmer-index --db-path phg_v2.4.8.162_ZeaSyn/vcf_dbs/ --hvcf-dir phg_v2.4.8.162_ZeaSyn/output/vcf_files/

### Path finding
phg find-paths \
    --path-keyfile /group/jrigrp11/cstark/readMappedSynDHgbs_keyfile.txt \
    --hvcf-dir /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/vcf_files \
    --reference-genome /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/updated_assemblies/B73.fa \
    --path-type haploid \
    --output-dir /group/jrigrp11/cstark/synDH_vcfFilesImputed \
    --out-parents-dir /group/jrigrp11/cstark/synDH_likelyAncestors

#### Look to see if there are actually overlapping parent calls
#### Look into collapsing regions of consensus
#### Upload graphing to slack channel



### GBS reads here: /group/jrigrp10/synthetic_load/fastq_data/synDH_fastq

#### keyfile is this structure. can support single reads (just filename not filename2)
#sampleName	filename	filename2
#sample1	read1.fq	read2.fq
#sample2	read3.fq	read4.fq
#sample3	read5.fq
