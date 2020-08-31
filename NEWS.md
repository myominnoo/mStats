---
output: github_document
---

## Changes in `mStats` beta version 3.3.0

### Updated on August 31, 2020

* `expandFreq` - can be used to expand frequency-weighted table with multiple variables allowed. 

* `strate` - allows multiple variables for stratification. It needs individual-level data. 


### Updated on August 30, 2020

* `regress`, `logit` and `esttab`
    - Display format is now similar to STATA's output.
    - function name `logistic` is replaced with `logit` because of its length.
    - `esttab` is a wrapper function for regression models. It supports `regress` and `logit`. 
    
    
### Updated on August 29, 2020

* `mhor` & `mhrr`
    - the format of output is changed. 
    - added some statistics such as attributable or prevented fractions. 
    - for calculating unstratified risk ratio using `mhrr`, odds ratio is also reported for conveniences.

* `finalize` - gathers statistics in a publication-ready format. It can be exported to a CSV file, or copy to a Word document.

### Updated on August 15, 2020

* `tab` 
    - can generate a publication-ready table.
    - fixed row and column percentage error.
    - run time is 2x times faster now.
    - Chi-squared test does not implement Yates' continuity correction.

* `summ`
    - can generate a publication-ready table.
    - minimum and maximum numbers included in grouped summary
    
* `labelVar`
    - fixed the background coding for consistent result and efficient processing
    - can handle multiple variable-to-label inputs, e.g. var1 = "Variable 1"
    
* `append`
    - fixed text notification to be less text and more accurate
    
* `egen` - fixed cut-off point error. 

* `recode`
    - fixed the background coding for consistent result and efficient processing
    - can handle multiple old-to-new-value inputs, e.g. "old value"-"new value"
    
* `histogram` 
    - can plot histogram with overlay normal curve
        
* `countBy` - This generates serial numbers per observation or per group. It is based on STATA's built-in system variables, `_n` and `_N`. 

* `lagRows` - can be used to retrieve the last observation per group.  

* `pyramid` - draws population pyramid graph by age and sex. This simplified function is based on the work of Minato Nakazawa who is the author of `pyramid` [package](https://cran.r-project.org/web/packages/pyramid/index.html). 

### Retired functions

* The following functions were removed because better versions of such functions can be used from the package `tidyverse`. 
- `arrange`
- `filter`
- `keep`
- `rename`
- `leftJoin`
    
