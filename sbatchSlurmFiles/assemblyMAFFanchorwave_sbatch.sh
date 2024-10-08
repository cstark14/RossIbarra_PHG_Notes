#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/
#SBATCH -J assemblyMAFFanchorwave
#SBATCH -o /group/jrigrp11/cstark/phgSbatchLogs/out_assemblyMAFFanchorwave.txt
#SBATCH -e /group/jrigrp11/cstark/phgSbatchLogs/error_assemblyMAFFanchorwave.txt
#SBATCH -t 24:00:00
#SBATCH --partition=high2

module load conda
module load apptainer
conda activate singularity
singularity run -B /group/jrigrp11/cstark/:/mnt --pwd /mnt phg16.simg /tassel-5-standalone/run_pipeline.pl -configParameters phg/maff_from_anchorwave_config.txt -AssemblyMAFFromAnchorWavePlugin -endPlugin

##### Manual designation of options/parameters. Not Complete. ended up being able to just create the maff assembly folder and that solved the issue.
#singularity run -B /group/jrigrp11/cstark/:/mnt --pwd /mnt phg16.simg /tassel-5-standalone/run_pipeline.pl -AssemblyMAFFromAnchorWavePlugin -outputDir phg/inputDir/assemblies/maff -keyFile phg/AssemblyMAFFromAnchorWavePlugin_keyfile.txt -gffFile phg/inputDir/reference/Zm-B73-REFERENCE-NAM-5.0_Zm00001eb.1.gff3