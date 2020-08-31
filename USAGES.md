
# mStats: Usages

## Usage

``` r
library(mStats)

## data management 
## check data structure and labels
codebook(infert)
```

    ## _______________________________________________________ 
    ## 
    ## Codebook: 'infert' 
    ## _______________________________________________________ 
    ## 
    ##  No       Variable | Label    Type Obs_Num <NA> <NA>(%)
    ##  -- -------------- + ----- ------- ------- ---- -------
    ##   1      education |  <NA>  factor     248    0       0
    ##   2            age |  <NA> numeric     248    0       0
    ##   3         parity |  <NA> numeric     248    0       0
    ##   4        induced |  <NA> numeric     248    0       0
    ##   5           case |  <NA> numeric     248    0       0
    ##   6    spontaneous |  <NA> numeric     248    0       0
    ##   7        stratum |  <NA> integer     248    0       0
    ##   8 pooled.stratum |  <NA> numeric     248    0       0
    ##  -- -------------- + ----- ------- ------- ---- -------
    ## _______________________________________________________

``` r
## label data and variable
infert1 <- labelVar(infert, 
                    education="Education in years", 
                    age="Age in years")
```

    ## ('education' labelled as 'Education in years')
    ## ('age' labelled as 'Age in years')

``` r
infert1 <- labelData(infert1, "Infertility after Abortion")
```

    ## (Dataset 'infert1': labelled as 'Infertility after Abortion')

``` r
codebook(infert1)
```

    ## ____________________________________________________________________ 
    ## 
    ## Codebook: 'infert1' 
    ## ____________________________________________________________________ 
    ## 
    ##  No       Variable |              Label    Type Obs_Num <NA> <NA>(%)
    ##  -- -------------- + ------------------ ------- ------- ---- -------
    ##   1      education | Education in years  factor     248    0       0
    ##   2            age |       Age in years numeric     248    0       0
    ##   3         parity |               <NA> numeric     248    0       0
    ##   4        induced |               <NA> numeric     248    0       0
    ##   5           case |               <NA> numeric     248    0       0
    ##   6    spontaneous |               <NA> numeric     248    0       0
    ##   7        stratum |               <NA> integer     248    0       0
    ##   8 pooled.stratum |               <NA> numeric     248    0       0
    ##  -- -------------- + ------------------ ------- ------- ---- -------
    ## ____________________________________________________________________ 
    ## (Dataset Label: Infertility after Abortion)

``` r
## alternatively use piping operation %>% from magrittr
library(magrittr)
infert %>% 
  labelVar(education="Education in years",
           age="Age in years",
           parity="count",
           induced="Number of prior induced abortions",
           case="case status",
           spontaneous="Number of prior spon. abortions",
           stratum="matched set number", 
           pooled.stratum="stratum number") %>% 
  labelData("Infertility after Abortion") %>% 
  codebook
```

    ## ('education' labelled as 'Education in years')
    ## ('age' labelled as 'Age in years')
    ## ('parity' labelled as 'count')
    ## ('induced' labelled as 'Number of prior induced abortions')
    ## ('case' labelled as 'case status')
    ## ('spontaneous' labelled as 'Number of prior spon. abortions')
    ## ('stratum' labelled as 'matched set number')
    ## ('pooled.stratum' labelled as 'stratum number')
    ## (Dataset '.': labelled as 'Infertility after Abortion')
    ## ___________________________________________________________________________________ 
    ## 
    ## Codebook: '.' 
    ## ___________________________________________________________________________________ 
    ## 
    ##  No       Variable |                             Label    Type Obs_Num <NA>
    ##  -- -------------- + --------------------------------- ------- ------- ----
    ##   1      education |                Education in years  factor     248    0
    ##   2            age |                      Age in years numeric     248    0
    ##   3         parity |                             count numeric     248    0
    ##   4        induced | Number of prior induced aborti... numeric     248    0
    ##   5           case |                       case status numeric     248    0
    ##   6    spontaneous | Number of prior spon. abortion... numeric     248    0
    ##   7        stratum |                matched set number integer     248    0
    ##   8 pooled.stratum |                    stratum number numeric     248    0
    ##  -- -------------- + --------------------------------- ------- ------- ----
    ##  <NA>(%)
    ##  -------
    ##        0
    ##        0
    ##        0
    ##        0
    ##        0
    ##        0
    ##        0
    ##        0
    ##  -------
    ## ___________________________________________________________________________________ 
    ## (Dataset Label: Infertility after Abortion)
