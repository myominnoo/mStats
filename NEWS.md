# mStats 3.2.3 (in development)

* Changed to appear fixed rounding decimal in `tab`.

# mStats 3.2.2 

* Updated references in description text and removed `\dontrun{}` in the example 

# mStats 3.2.1

## Bug fixes

* In `generate`, message is consistently displayed when subsetting `.data` results in vector. 

* In `leftJoin`, name of master dataset appear correctly on both title and footnote.

* Output of `logistic` and `regress` contains `model` as second and third lists respectively. This is intended for extended use of the functions.

* The `export` accommodates the inclusion of `model` and will skip if the output is model, either `glm` or `lm`.

* Heading of `esttab` is changed to the format `m#`. This prevents disruption of texts when written to` .csv` format.

* Formula to calculate 95% confidence interval for risk difference in `mhrr` is corrected. 

* Title of `linear` regression in `regress` is fixed from `logistic` to `linear`. 


## New function 

* `logistic` performs logistic regression and reports several associated statistics. 

* `regress` computes linear regresssion and generates linears statistics along with the option for robust standard errors. 

* `esttab` compiles regression estimates into in a table format seen in many journal articles. 

* `lagRows()` creates lagged version of variables. See `?lagRows`.


## to-do-lists 

* fix multiple variable options in `mhor` and `mhrr` 

* add plot for `strate` 

* add functions for `stmh` (MH rates: ratio and difference)

* add functions to calculate `RR` for binary outcome (also `poisson` regression)

* add functions for survival analysis. 

* revise `diagTest() `
