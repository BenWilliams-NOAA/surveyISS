# surveyISS

An R package for esimating input sample size for composition data provided by the AFSC bottom trawl surveys
*Note that this package is only useful when you have access to the AKFIN network and permissions to access the databases.*  

## Welcome 

This R package generates bootstrap replicates of the standard design-based indices of length and age composition and conditional age-at-length from AFSC bottom trawl survey data. 
These bootstrap replicates are then used to estimate the input sample size (ISS) for these data sources.
Survey regions include: Gulf of Alaska, Aleutian Islands, Eastern Bering Sea Shelf, Eastern Bering Sea Slope, and Northern Bering Sea Shelf.

### Installation:
Make sure you have installed R packages `devtools`.  
```
library(devtools)
devtools::install_github("BenWilliams-NOAA/surveyISS")
```

### Website: [afscdata](https://benwilliams-noaa.github.io/surveyISS/)

### Example Workflow:

The following is an example workflow to compute input sample size for a standard stock assessment. This example is provided for Gulf of Alaska Pacific cod.

First, after installing, load surveyISS package:
```
library(surveyISS)
```
Second, query data:
```
data <- surveyISS::query_data(survey = 47, 
                              region = 'goa', 
                              species = 21720, 
                              yrs = 1990)
```
Third, run srvy_iss() to get your ISS (for test run of 10 iterations):
```
surveyISS::srvy_iss(iters = 10, 
                    lfreq_data = data$lfreq,
                    specimen_data = data$specimen, 
                    cpue_data = data$cpue, 
                    strata_data = data$strata,
                    yrs = 1990,  
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'goa', 
                    save = 'test')
```
Finally, in the 'output/goa' folder you will see two output files, one with ISS for age composition, another for length composition.

There are a number of flexibilities built in the the surveyISS::srvy_iss() function (for example, setting plus age or length group, testing changes in age or length sample sizes, setting which output files you'd like to have), please see the function description for the various inputs to this function. 
There are also a number of special cases that have been developed, for example, spatially-explicit cases and stock complex cases (for both the AI and GOA), as well as being able to get ISS for conditional age-at-length data.
While the surveyISS package will produce estimates of age and length population that are also produced by GAP, we recommend (unless you're using a special case function) to get the age and length population estimates that are then used for age/length composition in your stock assessment from teh GAP_PRODUCTS tables hosted on AKFIN.


### NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. 
All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. 
Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. 
Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

### NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). 
The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. 
To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) |
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
