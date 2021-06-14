
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mStats <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version-ago/mStats)](https://cran.r-project.org/package=mStats)
[![Downloads](https://cranlogs.r-pkg.org/badges/mStats)](https://cran.r-project.org/package=mStats)
[![Lifecycle:
maturing](man/figures/lifecycle-maturing.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<!-- badges: end -->

mStats is a open-source R package to facilitate data management,
analysis and report writing with a focus on health research. It allows
to manipulate data, run statistical analysis, and calculate common
epidemiological measures. Their outputs, in turn, can be further
processed by highly sensible functions to produce publication-ready
tables.

In a nutshell, mStats is designed to make data analysis practical, quick
and easy for health researchers to create the final report in time. You
can see it in action to [Get
Started](https://mmoo.netlify.app/r/mStats/).

## Installation

You can install the released version of mStats from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("mStats")
```

### Development version

If you want to use the development version of the bookdown package, you
can install the package from GitHub via the {remotes
package}(<https://remotes.r-lib.org/>):

``` r
remotes::install_github("myominnoo/mStats")
```

## Cheat Sheet

\[to add later\]

## Masking

The `mStats`package contains two functions (`append`, `replace`) that
have the same names (doing different operation) with base R packages
(`stats` and `base`). Loading the `mStats` masks the functions from base
R. It means that when you use `append` function, you are using the
function from `mStats`. To avoid this:

-   use the syntax `package::function()`, for example `base::append()`
    or `mStats::append()`.
-   remove `mStats` from the session using `detach(package:mStats)`.

## Usage

The easiest way to get started with mStats is to follow the guide
[here](https://myominnoo.github.io/mStats/). Below is a quick
demonstration of what mStats can do.

``` r
## to use pipe %>% for expression of sequential operations 
## Hence, no need to provide `data` argument in subsequent operations
library(magrittr) 

library(mStats)

## Describe dataset
codebook(infert)
#>   Codebook
#>            Name  : infert
#>            Label : 
#>            Vars  : 8
#>            Obs   : 248
#>  + -- -------------- + ----- ------- --- -- ----- +
#>  | No       Variable | Label    Type Obs NA NA(%) |
#>  + -- -------------- + ----- ------- --- -- ----- +
#>  |  1      education |        factor 248  0   0.0 |
#>  |  2            age |       numeric 248  0   0.0 |
#>  |  3         parity |       numeric 248  0   0.0 |
#>  |  4        induced |       numeric 248  0   0.0 |
#>  |  5           case |       numeric 248  0   0.0 |
#>  |  6    spontaneous |       numeric 248  0   0.0 |
#>  |  7        stratum |       integer 248  0   0.0 |
#>  |  8 pooled.stratum |       numeric 248  0   0.0 |
#>  + -- -------------- + ----- ------- --- -- ----- +
```

Below, `infert` dataset is passed to the function `codebook` via piping
`%>%`. Note that dataset passed through piping is assigned as object `.`
in this operation.

``` r
infert %>% 
  codebook
#>   Codebook
#>            Name  : .
#>            Label : 
#>            Vars  : 8
#>            Obs   : 248
#>  + -- -------------- + ----- ------- --- -- ----- +
#>  | No       Variable | Label    Type Obs NA NA(%) |
#>  + -- -------------- + ----- ------- --- -- ----- +
#>  |  1      education |        factor 248  0   0.0 |
#>  |  2            age |       numeric 248  0   0.0 |
#>  |  3         parity |       numeric 248  0   0.0 |
#>  |  4        induced |       numeric 248  0   0.0 |
#>  |  5           case |       numeric 248  0   0.0 |
#>  |  6    spontaneous |       numeric 248  0   0.0 |
#>  |  7        stratum |       integer 248  0   0.0 |
#>  |  8 pooled.stratum |       numeric 248  0   0.0 |
#>  + -- -------------- + ----- ------- --- -- ----- +
```

Here we demonstrate two sets of operations: labeling dataset and
variables.

``` r
## Label variables and dataset
## new object is assigned to make the changes permanent
infert_demo <- infert %>% 
  label("Infertility after Spontaneous and Induced Abortion") %>% 
  label(education = "Mother's education",
        parity = "No of pregnant",
        induced = "No of prior induced abortions", 
        case = "case status") 
#>   (`.` labeled as `Infertility after Spontaneous and Induced Abortion`)
#>   (`education` labeled as `Mother's education`)
#>   (`parity` labeled as `No of pregnant`)
#>   (`induced` labeled as `No of prior induced abortions`)
#>   (`case` labeled as `case status`)

codebook(infert_demo)
#>   Codebook
#>            Name  : infert_demo
#>            Label : Infertility after Spontaneous and Induced Abortion
#>            Vars  : 8
#>            Obs   : 248
#>  + -- -------------- + ----------------------------- ------- --- -- ----- +
#>  | No       Variable |                         Label    Type Obs NA NA(%) |
#>  + -- -------------- + ----------------------------- ------- --- -- ----- +
#>  |  1      education |            Mother's education  factor 248  0   0.0 |
#>  |  2            age |                               numeric 248  0   0.0 |
#>  |  3         parity |                No of pregnant numeric 248  0   0.0 |
#>  |  4        induced | No of prior induced abortions numeric 248  0   0.0 |
#>  |  5           case |                   case status numeric 248  0   0.0 |
#>  |  6    spontaneous |                               numeric 248  0   0.0 |
#>  |  7        stratum |                               integer 248  0   0.0 |
#>  |  8 pooled.stratum |                               numeric 248  0   0.0 |
#>  + -- -------------- + ----------------------------- ------- --- -- ----- +
```

Next, we tabulate the `infert_demo` using the variables we processed
above and put it into `table1`. We do the same process but use `case` as
stratification variable to produce cross-tabulation.

``` r
## Tabulate variables by name as well as `:` operator:
table1 <- tab(infert_demo, education, parity:induced) 
#>  One-way tabulation
#>  + --------- ------- + ----- ------- ----- +
#>  |  Variable   Level | Freq. Percent  Cum. |
#>  + --------- ------- + ----- ------- ----- +
#>  | education         |                     |
#>  |            0-5yrs |    12     4.8   4.8 |
#>  |           6-11yrs |   120    48.4  53.2 |
#>  |           12+ yrs |   116    46.8 100.0 |
#>  +           ------- + ----- ------- ----- +
#>  |             Total |   248   100.0 100.0 |
#>  + --------- ------- + ----- ------- ----- +
#>  |    parity         |                     |
#>  |                 1 |    99    39.9  39.9 |
#>  |                 2 |    81    32.7  72.6 |
#>  |                 3 |    36    14.5  87.1 |
#>  |                 4 |    18     7.3  94.4 |
#>  |                 5 |     6     2.4  96.8 |
#>  |                 6 |     8     3.2 100.0 |
#>  +           ------- + ----- ------- ----- +
#>  |             Total |   248   100.0 100.0 |
#>  + --------- ------- + ----- ------- ----- +
#>  |   induced         |                     |
#>  |                 0 |   143    57.7  57.7 |
#>  |                 1 |    68    27.4  85.1 |
#>  |                 2 |    37    14.9 100.0 |
#>  +           ------- + ----- ------- ----- +
#>  |             Total |   248   100.0 100.0 |
#>  + --------- ------- + ----- ------- ----- +
#>  (education: Mother's education)
#>  (parity: No of pregnant)
#>  (induced: No of prior induced abortions)

## Tabulate variables stratified by a stratification variable: 
table2 <- tab(infert_demo, education, parity:induced, by = case) 
#>  Two-way tabulation
#>                               case
#>  + --------- ------- + --- ---- -- ---- ----- ----- ----- ----- +
#>  |  Variable   Level |   0 r(%)  1 r(%) Total  r(%)  chi2 exact |
#>  + --------- ------- + --- ---- -- ---- ----- ----- ----- ----- +
#>  | education         |                                          |
#>  |            0-5yrs |   8 66.7  4 33.3    12 100.0 0.999 1.000 |
#>  |           6-11yrs |  80 66.7 40 33.3   120 100.0             |
#>  |           12+ yrs |  77 66.4 39 33.6   116 100.0             |
#>  +           ------- + --- ---- -- ---- ----- ----- ----- ----- +
#>  |             Total | 165 66.5 83 33.5   248 100.0             |
#>  + --------- ------- + --- ---- -- ---- ----- ----- ----- ----- +
#>  |    parity         |                                          |
#>  |                 1 |  66 66.7 33 33.3    99 100.0 1.000 1.000 |
#>  |                 2 |  54 66.7 27 33.3    81 100.0             |
#>  |                 3 |  24 66.7 12 33.3    36 100.0             |
#>  |                 4 |  12 66.7  6 33.3    18 100.0             |
#>  |                 5 |   4 66.7  2 33.3     6 100.0             |
#>  |                 6 |   5 62.5  3 37.5     8 100.0             |
#>  +           ------- + --- ---- -- ---- ----- ----- ----- ----- +
#>  |             Total | 165 66.5 83 33.5   248 100.0             |
#>  + --------- ------- + --- ---- -- ---- ----- ----- ----- ----- +
#>  |   induced         |                                          |
#>  |                 0 |  96 67.1 47 32.9   143 100.0 0.964 0.969 |
#>  |                 1 |  45 66.2 23 33.8    68 100.0             |
#>  |                 2 |  24 64.9 13 35.1    37 100.0             |
#>  +           ------- + --- ---- -- ---- ----- ----- ----- ----- +
#>  |             Total | 165 66.5 83 33.5   248 100.0             |
#>  + --------- ------- + --- ---- -- ---- ----- ----- ----- ----- +
#>  (education: Mother's education)
#>  (parity: No of pregnant)
#>  (induced: No of prior induced abortions)
#>  (case: case status)
```

We can make a summary table that produces publication-ready tables.

``` r
## Summary table for simple tabulation
summary(table1)
#>                         Variable   Level Total\n n (%)
#> 1                          Total           248 (100.0)
#> 2             Mother's education                      
#> 3                                 0-5yrs      12 (4.8)
#> 4                                6-11yrs    120 (48.4)
#> 5                                12+ yrs    116 (46.8)
#> 6                 No of pregnant                      
#> 7                                      1     99 (39.9)
#> 8                                      2     81 (32.7)
#> 9                                      3     36 (14.5)
#> 10                                     4      18 (7.3)
#> 11                                     5       6 (2.4)
#> 12                                     6       8 (3.2)
#> 13 No of prior induced abortions                      
#> 14                                     0    143 (57.7)
#> 15                                     1     68 (27.4)
#> 16                                     2     37 (14.9)

## Summary table for cross-tabulation
summary(table2)
#>                         Variable   Level Total\n n (%)  0\n n (%) 1\n n (%)
#> 1                          Total           248 (100.0) 165 (66.5) 83 (33.5)
#> 2             Mother's education                                           
#> 3                                 0-5yrs    12 (100.0)   8 (66.7)  4 (33.3)
#> 4                                6-11yrs   120 (100.0)  80 (66.7) 40 (33.3)
#> 5                                12+ yrs   116 (100.0)  77 (66.4) 39 (33.6)
#> 6                 No of pregnant                                           
#> 7                                      1    99 (100.0)  66 (66.7) 33 (33.3)
#> 8                                      2    81 (100.0)  54 (66.7) 27 (33.3)
#> 9                                      3    36 (100.0)  24 (66.7) 12 (33.3)
#> 10                                     4    18 (100.0)  12 (66.7)  6 (33.3)
#> 11                                     5     6 (100.0)   4 (66.7)  2 (33.3)
#> 12                                     6     8 (100.0)   5 (62.5)  3 (37.5)
#> 13 No of prior induced abortions                                           
#> 14                                     0   143 (100.0)  96 (67.1) 47 (32.9)
#> 15                                     1    68 (100.0)  45 (66.2) 23 (33.8)
#> 16                                     2    37 (100.0)  24 (64.9) 13 (35.1)
#>    p-value\n (Chi2) p-value\n (Exact)
#> 1                                    
#> 2             0.999             1.000
#> 3                                    
#> 4                                    
#> 5                                    
#> 6             1.000             1.000
#> 7                                    
#> 8                                    
#> 9                                    
#> 10                                   
#> 11                                   
#> 12                                   
#> 13            0.964             0.969
#> 14                                   
#> 15                                   
#> 16
```

We can combine `table1` and `table2` into a more compact table. Note
that we use flextable package to make it more presentable.

``` r
## save two summaries 
sum1 <- summary(table1) 
sum2 <- summary(table2)

compact(sum1, sum2)
#>                         Variable   Level Total\n n (%)  0\n n (%) 1\n n (%)
#> 1                          Total           248 (100.0) 165 (66.5) 83 (33.5)
#> 2             Mother's education                                           
#> 3                                 0-5yrs      12 (4.8)   8 (66.7)  4 (33.3)
#> 4                                6-11yrs    120 (48.4)  80 (66.7) 40 (33.3)
#> 5                                12+ yrs    116 (46.8)  77 (66.4) 39 (33.6)
#> 6                 No of pregnant                                           
#> 7                                      1     99 (39.9)  66 (66.7) 33 (33.3)
#> 8                                      2     81 (32.7)  54 (66.7) 27 (33.3)
#> 9                                      3     36 (14.5)  24 (66.7) 12 (33.3)
#> 10                                     4      18 (7.3)  12 (66.7)  6 (33.3)
#> 11                                     5       6 (2.4)   4 (66.7)  2 (33.3)
#> 12                                     6       8 (3.2)   5 (62.5)  3 (37.5)
#> 13 No of prior induced abortions                                           
#> 14                                     0    143 (57.7)  96 (67.1) 47 (32.9)
#> 15                                     1     68 (27.4)  45 (66.2) 23 (33.8)
#> 16                                     2     37 (14.9)  24 (64.9) 13 (35.1)
#>    p-value\n (Chi2) p-value\n (Exact)
#> 1                                    
#> 2             0.999             1.000
#> 3                                    
#> 4                                    
#> 5                                    
#> 6             1.000             1.000
#> 7                                    
#> 8                                    
#> 9                                    
#> 10                                   
#> 11                                   
#> 12                                   
#> 13            0.964             0.969
#> 14                                   
#> 15                                   
#> 16
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/myominnoo/mStats/issues). For questions and
other discussion, please directly email me
[dr.myominnoo@gmail.com](mailto::dr.myominnoo@gmail.com) or use the
[mStats mailing list](https://groups.google.com/g/mstats).

------------------------------------------------------------------------

## The MIT License (MIT)

Copyright © 2013-2019 RStudio and others.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

------------------------------------------------------------------------

Please note that this project is looking for contributors. By
participating in this project, you agree to abide by its terms with
[Contributor Code of
Conduct](https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/),
version 1.0.0, available at
<https://www.contributor-covenant.org/version/1/0/0/code-of-conduct/>.
