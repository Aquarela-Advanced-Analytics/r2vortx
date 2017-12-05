# r2vortx
This package is an easy way to integrate(automate) R programs using the Aquarela VORTX plataform. In other words, R programas can use r2vortx functions to send and receive jobs to VORTX.

The main functions are:
 - **vortx_organizer**: Creates a job in VORTX **AND** runs Organizer, which peforms a pure automated clustering and outlier detection process while extracting the measurements of systemic importance of each factor/variable.
 - **vortx_discoverer**: Creates a job in VORTX **AND** runs Discoverer which is used for Business Scenario Discovery, which automatically shows the systemic combination of factors/variables related to a certain goal, key drivers and wheights of each variable.
 - **vortx_dataset**: Gets dataset of a processed job in VORTX.
 - **vortx_info**: Gets all available information from a processed job in VORTX.

Arguments and usage can be found in the documentation. Every function requires an API Key from VORTX, 
so it is recommended to add it as a variable in your programming environment.

## 01 - Basic Requirements

To use this package you will need to: 
  - Have an autorized VORTX API key 
  - httr package installed
  
## 02 - Usage

To use the package, you need to add the following command:
```
library("r2vortx")
```

## 03 - Installation

This package can be installed via devtools
```
install.packages('devtools')
devtools::install_github('edvalente/r2vortx')
```

## 04 - API - Information
All api addresses of VORTX can be found at api.vortx.io 

Contributors of this project should get in touch to get over Github to get the APIkeys. 

## How does VORTX work with spreadsheets? 
Here is a simple video showing this operation - https://www.youtube.com/watch?v=jifOmAX4-qI

## More information
Follow us at www.aquare.la 
