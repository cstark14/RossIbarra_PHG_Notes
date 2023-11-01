#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/
#SBATCH -J gvcfChrCombine
#SBATCH -o /group/jrigrp11/cstark/phgSbatchLogs/out_gvcfChrCombine.txt
#SBATCH -e /group/jrigrp11/cstark/phgSbatchLogs/error_gvcfChrCombine.txt
#SBATCH -t 24:00:00
#SBATCH --partition=bigmemm
#SBATCH --mem=524000 
#SBATCH --mincpus=16
# mail alerts at beginning and end of job
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
# send mail here
#SBATCH --mail-user=crstark@ucdavis.edu

module load conda apptainer gatk deprecated/java tabix bcftools
#conda activate singularity

#singularity run -B /group/jrigrp11/cstark/:/mnt --pwd /mnt phg16.simg java -Xmx200g -XX:ParallelGCThreads=10 -jar /share/apps/22.04/spack/spack-v0.19.1/opt/spack/linux-ubuntu22.04-x86_64_v2/gcc-11.3.0/gatk-4.2.6.1-xjwogmjj4lbik4vhwckg2lasbgx54sl7/bin/gatk-package-4.2.6.1-local.jar GatherVcfs \
#gatk GatherVcfsCloud \
bcftools concat -a -o phg/inputDir/loadDB/gvcf/gatkNAMnotTzi_B73v5_allChr.vcf.gz\
phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_1.gvcf.gz \
phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_2.gvcf.gz \
phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_3.gvcf.gz \
phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_4.gvcf.gz \
phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_5.gvcf.gz \
phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_6.gvcf.gz \
phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_7.gvcf.gz \
phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_8.gvcf.gz \
phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_9.gvcf.gz \
phg/inputDir/loadDB/gvcf/namNotTzi_perChr/gatkNAM_B73v5_10.gvcf.gz 
#-O phg/inputDir/loadDB/gvcf/gatkNAMnotTzi_B73v5_allChr.vcf.gz
tabix phg/inputDir/loadDB/gvcf/gatkNAMnotTzi_B73v5_allChr.vcf.gz