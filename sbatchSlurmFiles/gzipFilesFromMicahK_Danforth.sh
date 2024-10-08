#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/filesFromMicahK_Danforth/
#SBATCH -J downloadingFilesFromMicahDanforth
#SBATCH -o /group/jrigrp11/cstark/filesFromMicahK_Danforth/out-%A_download.txt
#SBATCH -e /group/jrigrp11/cstark/filesFromMicahK_Danforth/error-%A_download.txt
#SBATCH -t 24:00:00
#SBATCH --partition=high2

gzip /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn_fromMicahKDanforth_20241004.tar