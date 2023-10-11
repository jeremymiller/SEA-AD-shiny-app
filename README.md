# SEA-AD-shiny-app
Welcome to the AD Gene Expression Trajectory Viewer, a web application for exploring how genes change expression with increasing Alzheimer’s Disease pathology in different cell types. It relies on two key pieces of information:

Quantitative Neuropathology
Donor brain tissue sections from the middle temporal gyrus (MTG) are stained for key pathological proteins and cell types of interest to Alzheimer’s disease. We use machine learning to quantify the staining on these images and define a single continuous pseudo-progression score that can order donors along a trajectory from least AD pathology to most AD pathology.

Gene Expression
Single nucleus RNA-sequencing was collected for ~1.7 million cells and used to define 151 sets of cells with distinct gene expression profiles (supertypes). We compare gene expression levels in cells from each supertype with continuous pseudo-progression scores to identify genes changes in AD.

This tool organizes more than 6 million sets of images exploring every combination of gene and cell type at multiple taxonomy levels. We hope you find this tool useful. Please feel free to provide feedback at this [link](https://app.smartsheet.com/b/form/01c0ed3a74d14135bd68620a92bbd5ef). 

Here is the link to the app, https://sea-ad.shinyapps.io/ad_gene_trajectories/.



## TODO:
- [ ] Add population as a dropdown under subclass and supertype
- [ ] Check on naming incosistencies between table and figure content (GABA vs Inh)
- [ ] Correct Typo in description
- [ ] Remove the serial numbers 1,2,3 in the descritpion or resize them

