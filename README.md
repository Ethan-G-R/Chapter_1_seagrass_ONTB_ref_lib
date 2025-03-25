# Chapter_1_seagrass_ONTB_ref_lib

This repository contains all the sequence data used to generate a DNA barcode reference library for seagrass associated invertebrates of Scotland using ONTBarcoder2  
preprint 10.22541/au.174049397.70710513/v1

**Repo guide**  
All outputs from ONTBarcoder2 can be found in ONTB2_outputs/ONTBarcoder2Output/  
  
BLAST and BOLD hits used to identify each sequence can be found in BLAST_BOLD_outputs  
  
R scripts used to obtain summary counts and statistics can be found in summary_counts/   
*Chapter_1_summary_condenced.xlsx is a summary of the entrire dataset*  
  
R scripts for producing graphics can be found in plot/FINAL_plots.R  

**MAPPING**  
The mapping step was carried out using Geneious Prime version 2024.0.7  
Input reads to Geneious Prime for mapped can be found ONTB2_outputs/ONTBarcoder2Output/demultiplexed  
Map sequences can be found in mapped_reruns/geneious_mapping_record  
Mapped reads can be found in mapped_reruns/*_rerun  


**Naming convention**  
raw sequences = sequences which ONTBarcoder2 produced with no modification  
mapped sequences = sequences which ONTBarcoder2 produced after a subset of reads were mapped to a map sequence in Geneious Prime  
