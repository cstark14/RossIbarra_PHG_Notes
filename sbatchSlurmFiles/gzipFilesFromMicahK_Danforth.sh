#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/filesFromMicahK_Danforth/
#SBATCH -J gzipFilesFromMicahDanforth
#SBATCH -o /group/jrigrp11/cstark/filesFromMicahK_Danforth/out-%A_gzip.txt
#SBATCH -e /group/jrigrp11/cstark/filesFromMicahK_Danforth/error-%A_gzip.txt
#SBATCH -t 24:00:00
#SBATCH --partition=high2

gzip /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn_fromMicahKDanforth_20241004.tar