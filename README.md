# r2vortx
This project aims at generating good quality R codes that facilitates R users to connect, access and play arround with Aquarela VORTX machine-learning plataform. 

# Documentation

"There is a strong correlation between excelent software and excelent software documentation". So here is the place to get started. 

## API - Information

All api addresses of VORTX can be found at api.vortx.io 

Contributors of this project should get in touch to get over Github to get the APIkeys. 

## Projetct Dictionary

### R
The R directory contains various files with function definitions (but only function definitions - no code that actually runs). The code itself should be on the root directory. 

### Data
CSV file(s) used to make tests. This is treated as read only; in paricular the R files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.

### Docs
The doc directory contains the paper. I work in LaTeX which is nice because it can pick up figures directly made by R. Markdown can do the same and is starting to get traction among biologists. With Word you’ll have to paste them in yourself as the figures update.

### Figs
The figs directory contains the figures. This directory only contains generated files; that is, I should always be able to delete the contents and regenerate them.

### Outputs
Files that resulted from the processing including logs, simuation output, processed datasets, logs, or other processed things.


