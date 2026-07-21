## Libraries
library(data.table)
library(arrow)

## Read in the two tables
new = fread("effect_size_table.csv")  # https://sea-ad-single-cell-profiling.s3.amazonaws.com/MTG/RNAseq/Supplementary%20Information/Nebula%20Results/effect_size_table.csv
old = fread("beta_coefficient_table.csv")  # https://sea-ad-single-cell-profiling.s3.amazonaws.com/MTG/RNAseq/pseudoprogression-plots/beta_coefficient_table.csv

## Reformatting to the updated cell type names
old$Population <- gsub("L2 3","L2/3",old$Population)
old$Population <- gsub("L5 6","L5/6",old$Population)
old_name <- c("Micro-PVM_1_1-SEAAD", "Micro-PVM_2_2-SEAAD", "VLMC_2_1-SEAAD", "VLMC_2_2-SEAAD", "VLMC_2",
             "Excitatory","Inhibitory","Non-neuronal")
new_name <- c("Monocyte", "Lymphocyte", "SMC-SEAAD", "Pericyte_2-SEAAD", "Pericyte_1", 
              "Neuronal: Glutamatergic", "Neuronal: GABAergic", "Non-neuronal and non-neural")
for (i in 1:length(old_name))  
  old$Population[old$Population==old_name[i]] = new_name[i]
  
## Align the indexes for the two tables
index_new = paste(new$Gene,new$`Taxonomy Level`,new$Population)
index_old = paste(old$Gene,old$`Taxonomy Level`,old$Population)
old_match = old[match(index_new,index_old),]

## Overwrite the old beta scores with the new beta scores
cols <- colnames(new)[5:7]
colnames(old_match)[5:7] <- cols
for (col in cols)
  old_match[[col]] <- new[[col]]

## Remove row names and save as a feather file called output_new.feather
table_out <- old_match[,2:10]
write_feather(table_out,"output_new.feather")
