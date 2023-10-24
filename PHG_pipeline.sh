#!/bin/bash

## This is a way of catalouging all singularity commands that go into preproccessing, constructing, and making use of a PHG.
## I would not recomend running all of these commands at once, especially due to the significant logging output resulting from each command.

## General command syntax:
## - phg16.simg is the singularity image file queried in singularity commands. Everything following is a PHG command, Tassel 5 is the pipeline stored in the singularity image.
## - Config files following -configParameters are often extremely complicated and will come with their own annotation.
## - At certain points in this pipeline the PHG database will be irrevocably changed, I would strongly recomend archiving versions of your database following loading haplotypes from GVCF files, creating consensus haplotypes, and after imputing.

### getting phg into UCD Farm HPC
module load apptainer
module load conda
conda build -n singularity -c conda-forge singularity
singularity build phg16.simg docker://maizegenetics/phg:0.0.40

# Creation of default directory structure. Only needs to be run once.
# -workingDir is a path directory specified by user.
singularity run -B /group/jrigrp11/cstark/:/mnt --pwd /mnt phg16.simg /tassel-5-standalone/run_pipeline.pl -debug -MakeDefaultDirectoryPlugin
 -workingDir phg -endPlugin

### convert raw bed gene file to noOverlap
singularity run -B /group/jrigrp11/cstark/:/mnt --pwd /mnt phg16.simg /tassel-5-standalone/run_pipeline.pl -Xmx50G -debug -CreateValidIntervalsFilePlugin -intervalsFile phg/inputDir/reference/B73v5_genes.bed -referenceFasta phg/inputDir/reference/Zm-B73-REFERENCE-NAM-5.0.fa -mergeOverlaps true -generatedFile phg/inputDir/reference/B73v5_genes_noOverlaps.bed -mergeOverlaps true -endPlugin > plugin_output.txt

#### https://stackoverflow.com/questions/65642199/difference-between-working-directory-of-docker-and-singularity
# Initial configuration of PHG. It is extremely vital that this command completes safely.
# - The foundational reference genome haplotypes should be loaded after this step.
# - Beware of a very common error, if the reference genome is already compressed this step will throw an error. Easiest solution is to remove and compressed version of the reference.
singularity run -B /group/jrigrp11/cstark/:/mnt --pwd /mnt phg16.simg /tassel-5-standalone/run_pipeline.pl -Xmx100G -debug -configParameters phg/initial_config.txt -MakeInitialPHGDBPipelinePlugin -endPlugin


# First alignment step necessary to create GVCF files from desired genomes. If you can create or aquire bgzipped files you can proceed to CreateHaplotypesFromGVCF.
singularity run -B /group/jrigrp11/cstark/:/mnt --pwd /mnt phg16.simg /tassel-5-standalone/run_pipeline.pl -configParameters phg/maff_from_anchorwave_config.txt -AssemblyMAFFromAnchorWavePlugin -endPlugin

# Second alignment step taking maff files created previously and outputting to gvcf.
#singularity run phg16.simg /tassel-5-standalone/run_pipeline.pl -Xmx100G -debug -configParameters phg/maft_to_gvcf_config.txt -MAFToGVCFPlugin -endPlugin

# This is a very delicate process. For our work certain hardcoded elements in the CreateHaplotypesFromGVCF file made it necessary to pull the file from the singularity image and edit it. I would first try the syntax used by other commands but if your pathing is erroneous feel free to mirror our methods.
singularity exec phg16.simg /shares/baxter/users/tkosfeld/working_PHG/CreateHaplotypesFromGVCF.groovy -config phg/gvcf_config.txt


# Creation of consensus haplotypes from already stored haplotypes. Keep in mind this is very parameter dependant. Also this long command is somewhat repetitive with specifications given the most recent config version.
singularity exec phg16.simg /tassel-5-standalone/run_pipeline.pl -Xmx100G -debug -configParameters phg/consensus_config.txt -HaplotypeGraphBuilderPlugin -configFile phg/consensus_config.txt -methods method2ref:assembly_by_anchorwave -includeVariantContexts true -localGVCFFolder phg/inputDir/loadDB/gvcf/ -endPlugin -RunHapConsensusPipelinePlugin -referenceFasta phg/inputDir/reference/Ref.fa -dbConfigFile phg/consensus_config.txt -collapseMethod consensus -collapseMethodDetails assembly_by_anchorwave_to_consensus -rankingFile phg/ranking_file.txt -mxDiv 0.00001 -clusteringMode kmer_assembly -isTestMethod true -endPlugin

# First step of imputation, creation of the pangenome from all specified haplotypes.
singularity exec phg16.simg /tassel-5-standalone/run_pipeline.pl -Xmx116G -debug -configParameters phg/imputation_config.txt -ImputePipelinePlugin -imputeTarget pangenome -endPlugin

# This is the actual imputation step, much like the consensus, this is extreme parameter dependant.
singularity exec phg16.simg /tassel-5-standalone/run_pipeline.pl -Xmx116G -debug -configParameters phg/imputation_config.txt -ImputePipelinePlugin -imputeTarget path -endPlugin

# Extraction of SNPs from completed imputation database.
singularity exec phg16.simg /tassel-5-standalone/run_pipeline.pl -Xmx128G -debug -configParameters phg/imputation_config.txt -ImputePipelinePlugin -imputeTarget pathToVCF -endPlugin
