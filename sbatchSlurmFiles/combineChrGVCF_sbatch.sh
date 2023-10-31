#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/
#SBATCH -J gvcfChrCombine
#SBATCH -o /group/jrigrp11/cstark/phgSbatchLogs/out_gvcfChrCombine.txt
#SBATCH -e /group/jrigrp11/cstark/phgSbatchLogs/error_gvcfChrCombine.txt
#SBATCH -t 24:00:00
#SBATCH --partition=bigmemh
# mail alerts at beginning and end of job
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
# send mail here
#SBATCH --mail-user=crstark@ucdavis.edu

module load conda apptainer gatk
#java -Xmx200g -XX:ParallelGCThreads=10 -jar gatk GatherVcfs \
gatk GatherVcfs \
-I /phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_1.gvcf.gz \
-I /phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_2.gvcf.gz \
-I /phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_3.gvcf.gz \
-I /phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_4.gvcf.gz \
-I /phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_5.gvcf.gz \
-I /phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_6.gvcf.gz \
-I /phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_7.gvcf.gz \
-I /phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_8.gvcf.gz \
-I /phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_9.gvcf.gz \
-I /phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_10.gvcf.gz \
-O /phg/inputDir/loadDB/gvcf/gatkNAMnotTzi_B73v5_allChr.vcf.gz
tabix /phg/inputDir/loadDB/gvcf/gatkNAMnotTzi_B73v5_allChr.vcf.gz