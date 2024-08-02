# surveyISS
<!-- badges: start -->
[![R-CMD-check](https://github.com/BenWilliams-NOAA/surveyISS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BenWilliams-NOAA/surveyISS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


An R package for estimating input sample size for composition data provided by the AFSC bottom trawl surveys  
*Note that this package is only useful when you have access to the AKFIN network and permissions to access the databases.*  

## Welcome 

This R package generates bootstrap replicates of the standard design-based indices of length and age composition and conditional age-at-length from AFSC bottom trawl survey data. 
These bootstrap replicates are then used to estimate the input sample size (ISS) for these data sources.
The general bootstrap framework is described in [Hulson et al. 2023](https://repository.library.noaa.gov/view/noaa/50425) for age and length composition from AFSC bottom trawl surveys, and in [Hulson and Williams 2024](https://doi.org/10.1016/j.fishres.2023.106894) for the inclusion of ageing error and growth variability, as well as the application to conditional age-at-length data.
Survey regions include: Gulf of Alaska, Aleutian Islands, Eastern Bering Sea Shelf, Eastern Bering Sea Slope, and Northern Bering Sea Shelf (noting that survey regions can be combined).

Please see the vignettes for examples of the overall workflow when using `surveyISS` as well as examples for special cases (e.g., stock complexes, spatially-explicit, and conditional age-at-length).

### Installation:
Make sure you have installed R packages `devtools`.  
```
library(devtools)
devtools::install_github("BenWilliams-NOAA/surveyISS")
```

### Website: [surveyISS](https://benwilliams-noaa.github.io/surveyISS/)

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
