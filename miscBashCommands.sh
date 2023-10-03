#### Creating bed from gff3
##### Jeff said to add 5kb to each edge?
wget https://download.maizegdb.org/Zm-B73-REFERENCE-NAM-5.0/Zm-B73-REFERENCE-NAM-5.0_Zm00001eb.1.gff3.gz
zcat Zm-B73-REFERENCE-NAM-5.0_Zm00001eb.1.gff3.gz | grep '#' > B73v5_genes.gff3
zcat Zm-B73-REFERENCE-NAM-5.0_Zm00001eb.1.gff3.gz | grep 'gene' >> B73v5_genes.gff3
cat B73v5_genes.gff3 | grep -v "#" | cut -f 1,4,5 > B73v5_genes.bed
