
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dtsurvey

<!-- badges: start -->
<!-- badges: end -->

dtsurvey is a partial implementation of `survey` package routines so
that `data.table` syntax can be used. Basic aggregations like means and
totals are implemented for both classic complex survey designs and
replicate designs. Fancier things like regressions, finite population
corrections (FPCs) and what not are not implemented.

## Installation

From [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PHSKC-APDE/dtsurvey")
```

## Example

Before survey calculations can be computed, a dataset must be declared
as a type of `dtsurvey` object:

``` r
library(dtsurvey)
library('survey', quietly = TRUE) # to get access to the datasets
#> 
#> Attaching package: 'survey'
#> The following object is masked from 'package:graphics':
#> 
#>     dotchart
data(api)

#survey vs. dtsurvey style, complex surveys
dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1)
dtclus1 <- dtsurvey(apiclus1, 'dnum', weight = 'pw')

#dtsurvey loosely wraps survey::svrepdesign for replicate designs
# You can either pass the arguments for svrepdesign through ..., or pass
# an existing svrepdesign
drep = as.svrepdesign(dclus1)
dtrep1 = dtrepsurvey(drep)

#To get access to dtsurvey routines, but with non survey data, use `dtadmin`
dtad = dtadmin(apiclus1)
```

Compute means/proportions with `smean` within a `[`

``` r
##means
#Standard complex survey
dtclus1[, smean(api00), by = stype] #grouping commands are easy
#>    stype       V1
#> 1:     H 618.5714
#> 2:     E 648.8681
#> 3:     M 631.4400
dtclus1[stype == 'E', smean(api00), by = both] #so is subsetting!
#>    both       V1
#> 1:  Yes 653.6250
#> 2:   No 632.2188
dtclus1[, smean(api00, var_type = c('se', 'ci'))] #adding error metrics
#>      result       se    lower    upper
#> 1: 644.1694 23.77901 593.1685 695.1703
#Replicate survey
dtrep1[, smean(api00), by = stype] #grouping commands are easy
#>    stype       V1
#> 1:     H 618.5714
#> 2:     E 648.8681
#> 3:     M 631.4400
dtrep1[stype == 'E', smean(api00), by = both] #so is subsetting!
#>    both       V1
#> 1:  Yes 653.6250
#> 2:   No 632.2188
dtrep1[, smean(api00, var_type = c('se', 'ci'))] #adding error metrics
#>      result       se    lower    upper
#> 1: 644.1694 26.59416 587.1306 701.2082
#Normal dataset. smean should be equal to `mean`
dtad[, smean(api00), by = stype] #grouping commands are easy
#>    stype       V1
#> 1:     H 618.5714
#> 2:     E 648.8681
#> 3:     M 631.4400
dtad[stype == 'E', smean(api00), by = both] #so is subsetting!
#>    both       V1
#> 1:  Yes 653.6250
#> 2:   No 632.2188
dtad[, smean(api00, var_type = c('se', 'ci'))] #adding error metrics
#>      result       se   lower    upper
#> 1: 644.1694 7.817181 628.848 659.4908
```

Basic column assignment is still possible after an object is case as a
dtsurvey/dtrepsurvey/dtadmin. While it is possible to assign the results
of `smean` (and `stotal`) using the `:=` approach, that should be used
sparingly. Factors and instances where `var_type != NULL` return weird
objects. Worth testing out before going too crazy.

``` r
dtclus1[, count := 1] # to estimate the total number of schools
dtrep1[, count := 1]
```

Compute totals with `stotal`

``` r
dtclus1[, stotal(count), by = stype]
#>    stype        V1
#> 1:     H  473.8579
#> 2:     E 4873.9675
#> 3:     M  846.1749
dtrep1[, stotal(count), by = stype]
#>    stype        V1
#> 1:     H  473.8579
#> 2:     E 4873.9675
#> 3:     M  846.1749
```

Factors work within `smean` and `stotal`, but can be weird:

``` r
class(dtclus1[, awards])
#> [1] "factor"
#How to identify which value belongs to what
dtclus1[, smean(awards)] #returns a named vector
#>        No       Yes 
#> 0.2896175 0.7103825
a = dtclus1[, smean(awards),stype] #also returns a named vector, but stripped
a
#>    stype        V1
#> 1:     H 0.5714286
#> 2:     H 0.4285714
#> 3:     E 0.2291667
#> 4:     E 0.7708333
#> 5:     M 0.4800000
#> 6:     M 0.5200000
names(a[, V1]) #ruhroh
#> NULL
#however, with some clever ordering, you should be able to recover the levels
dtclus1[, fff := factor(sample(c('A', 'B'), nrow(dtclus1), replace = T))]
dtclus1[stype == 'E', fff := 'A']
dtclus1[, .(smean(fff), levels(fff)), stype]
#>    stype        V1 V2
#> 1:     H 0.5714286  A
#> 2:     H 0.4285714  B
#> 3:     E 1.0000000  A
#> 4:     E 0.0000000  B
#> 5:     M 0.4000000  A
#> 6:     M 0.6000000  B
#NAs in the factor seem to work alright
dtclus1[stype == 'M' & fff == 'A', fff := NA]
dtclus1[, .(smean(fff), levels(fff)), stype]
#>    stype        V1 V2
#> 1:     H 0.5714286  A
#> 2:     H 0.4285714  B
#> 3:     E 1.0000000  A
#> 4:     E 0.0000000  B
#> 5:     M 0.0000000  A
#> 6:     M 1.0000000  B
#factors with ses and cis
#because multiple columns are being returned, a "levels" column comes along
# for the ride
dtclus1[, smean(fff, var_type = c('se', 'ci')), stype]
#>    stype    result        se     lower     upper levels
#> 1:     H 0.5714286 0.1302199 0.2635075 0.8793497      A
#> 2:     H 0.4285714 0.1302199 0.1206503 0.7364925      B
#> 3:     E 1.0000000 0.0000000 1.0000000 1.0000000      A
#> 4:     E 0.0000000 0.0000000 0.0000000 0.0000000      B
#> 5:     M 0.0000000 0.0000000 0.0000000 0.0000000      A
#> 6:     M 1.0000000 0.0000000 1.0000000 1.0000000      B
```

When a CI is needed for proportions, additional methods of for
calculating CIs are available

``` r
#default borrowed from survey package (and general statistics)
dtclus1[, smean(awards, var_type = 'ci', ci_method = 'mean')]
#>       result     lower     upper levels
#> 1: 0.2896175 0.2180881 0.3611469     No
#> 2: 0.7103825 0.6388531 0.7819119    Yes
#See survey::svyciprop for more info about how these work
dtclus1[, smean(awards, var_type = 'ci', ci_method = 'xlogit')] #xlogit
#>       result     lower     upper levels
#> 1: 0.2896175 0.2235820 0.3659637     No
#> 2: 0.7103825 0.6340363 0.7764180    Yes
dtclus1[, smean(awards, var_type = 'ci', ci_method = 'beta')] #beta
#>       result     lower     upper levels
#> 1: 0.2896175 0.2199894 0.3673995     No
#> 2: 0.7103825 0.6326005 0.7800106    Yes
#dtadmin have their own method
dtad[, smean(awards, var_type = 'ci', ci_method = 'unweighted_binary'), stype]
#>    stype    result     lower     upper levels
#> 1:     H 0.5714286 0.3259064 0.7861920     No
#> 2:     H 0.4285714 0.2138080 0.6740936    Yes
#> 3:     E 0.2291667 0.1680899 0.3043179     No
#> 4:     E 0.7708333 0.6956821 0.8319101    Yes
#> 5:     M 0.4800000 0.3003129 0.6650148     No
#> 6:     M 0.5200000 0.3349852 0.6996871    Yes
#vary the level
dtclus1[, smean(awards, var_type = 'ci', ci_method = 'xlogit', level = .9)]
#>       result     lower     upper levels
#> 1: 0.2896175 0.2345579 0.3516638     No
#> 2: 0.7103825 0.6483362 0.7654421    Yes
dtclus1[, smean(awards, var_type = 'ci', ci_method = 'xlogit', level = .99)]
#>       result     lower     upper levels
#> 1: 0.2896175 0.2010428 0.3977867     No
#> 2: 0.7103825 0.6022133 0.7989572    Yes
```
