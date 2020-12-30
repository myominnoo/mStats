---
output: github_document
---
## Changes in `mStats` version 3.4.1

* `summary.summ`
    - UPDATED: fixed the error thrown when simple `summ` without `by` is used with `detail` set to `TRUE`.

* `summary.tab`
    - UPDATED: removed `Total` row for each variable and put one `Total` row at the first row of the table under headings.
    - UPDATED: `Total` column in cross-tabulation now comes first before stratification.

* `summary.logit`
    - NEW FUNCTION: generates ready-made tables from logistic regression model outputs. 


## Changes in `mStats` version 3.4.0

* all functions has been re-written for consistency and performance.

* `regress` and `logit` are all working under the same principle that the model has to be fitted before feeding it into `regress` and `logit`.

* S3 functions for tabulation and summary of the R base's `summary` is now updated and work consistently.



## Changes in `mStats` beta version 3.3.1

## Updated on September 5, 2020

* `hettest` - perform the Breusch-Pagan test for heteroskedasticity.

* `linkTest` - performs a link test for model specification.

* `plot.regress` - S3 method for plot. It generates four plots for linear model diagnostics

## Updated on September 4, 2020

* `scatterPlotMatrix` - creates a scatter plot matrices with 
    - scatter plots with smooth regression line in lower panel
    - histograms in diagonal panel 
    - correlation coefficients in upper panel
    
* `predict.regress` - S3 method for predicting `regress` output: returns original data with other regression diagnostic statistics including Cook's distance.

## Changes in `mStats` beta version 3.3.0

## Updated on September 2, 2020

* `tab` 
    - returns a list with `tab` for tabulation measures and `lbl` for variable labels
    - class is changed to `tab1` for one-way tabulation, `tab2` for two-way tab without percentage and tab2p for two-way with percentages. This is implemented in S3 methods for summary. 
    
* `printLabel` - returns a label value.


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
    
