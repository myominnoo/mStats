---
output: github_document
---

# mStats 3.3.0 (in development)

## Updates

* `tab` 
    - can generate a publication-ready table.
    - fixed row and column percentage error.
    - run time is 2x times faster now.

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
    
* `histogram` - can plot histogram with overlay normal curve
        
* `countBy` - This generates serial numbers per observation or per group. It is based on STATA's built-in system variables, `_n` and `_N`. 

* `lagRows` - can be used to retrieve the last observation per group.  

* functions removed because same functions in `tidyverse`
- `arrange`
- `filter`
- `keep`
- `rename`
- `leftJoin`
    
