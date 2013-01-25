Regression simulation function
===============================

A flexible suite of functions to simulate nested data.  
Currently supports the following featurs:
* Longitudinal data simulation
* Two levels of nesting
* Specification of distribution of random effects
* Specification of distribution of within cluster errors
* Specification of serial correlation
* Specification of population parameters
 *Including both fixed and random effects
* Specification of the number of variables
 * Ability to add time-varying covariates
 * Specify the mean and variance of fixed covariate variables
* Generation of mixture normal distributions
 * Ability to compute variance of each normal distribution in each mixture distribution
   based on equal weighting.
* Cross sectional data simulation
* Single level simulation

Features coming soon:
* Adding factor variable simulation
* More options for simulating random components
 * More distributions
 * Ability to simulate different distributions for different random effects
* Power by simulation
* Missing Data
* Ability to specify correlation amount random effects individually.
* Expand variance of mixture distribution function to include unequal weighting.

Single level simulation example
============================

Simulating a single level regression model with sim.reg.single, mean centered variables.  This function outputs the entire design matrix, the error, the cross product, simulated data, and the ID variable.


```r
library(simReg)
fixed <- ~1 + act + diff + numCourse + act:numCourse
fixed.param <- c(2, 4, 1, 3.5, 2)
cov.param <- list(c(0, 4), c(0, 3), c(0, 3))
n <- 150
errorVar <- 3
err.dist <- "norm"
head(temp.single <- sim.reg.single(fixed, fixed.param, cov.param, n, errorVar, 
    err.dist), n = 15)
```

```
##    X.Intercept.      act    diff numCourse act.numCourse    Fbeta     err
## 1             1 -8.78080  0.6765    4.3938      -38.5810 -94.2304  0.7828
## 2             1 -2.67596 -3.4624   -0.8662        2.3178 -10.5622  0.6236
## 3             1  0.17763 -2.1391    5.1301        0.9113  20.3492  1.1872
## 4             1 -2.81207  2.6916   -2.6497        7.4512  -0.9283 -1.5155
## 5             1 -0.89395 -0.5351    1.0958       -0.9796  -0.2348  2.2445
## 6             1 -9.22626 -0.9860    2.4982      -23.0491 -73.2455 -1.7834
## 7             1 -1.86310 -1.1012   -3.1370        5.8445  -5.8440  0.6786
## 8             1  7.15842  2.0487   -1.2724       -9.1087  10.0114  0.5383
## 9             1 -0.90465  6.8614    1.0626       -0.9613   7.0394  0.8845
## 10            1 -1.03928  1.3674    5.3209       -5.5299   6.7737 -1.1842
## 11            1 -0.08082  1.7356    1.6657       -0.1346   8.9732  1.1595
## 12            1  7.32316 -1.4790    1.7060       12.4933  60.7712 -1.5900
## 13            1 -1.56233  0.7064    0.4496       -0.7025  -3.3741  1.6044
## 14            1  0.25322  5.3180   -3.0140       -0.7632  -3.7443  1.2289
## 15            1 -2.29496  1.8939    0.5932       -1.3613  -5.9325  1.7530
##    sim.data ID
## 1   -93.448  1
## 2    -9.939  2
## 3    21.536  3
## 4    -2.444  4
## 5     2.010  5
## 6   -75.029  6
## 7    -5.165  7
## 8    10.550  8
## 9     7.924  9
## 10    5.589 10
## 11   10.133 11
## 12   59.181 12
## 13   -1.770 13
## 14   -2.515 14
## 15   -4.180 15
```

```r

## Fitting regression to obtain parameter estimates
summary(lm(sim.data ~ 1 + act + diff + numCourse + act:numCourse, data = temp.single))
```

```
## 
## Call:
## lm(formula = sim.data ~ 1 + act + diff + numCourse + act:numCourse, 
##     data = temp.single)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.575 -1.196  0.188  1.153  4.576 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     2.3828     0.1462    16.3   <2e-16 ***
## act             3.9653     0.0383   103.5   <2e-16 ***
## diff            1.0235     0.0518    19.8   <2e-16 ***
## numCourse       3.5285     0.0465    76.0   <2e-16 ***
## act:numCourse   2.0046     0.0114   175.5   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 1.78 on 145 degrees of freedom
## Multiple R-squared: 0.997,	Adjusted R-squared: 0.997 
## F-statistic: 1.37e+04 on 4 and 145 DF,  p-value: <2e-16
```


Multilevel simulation example
==========================

Longitudinal Data
-------------------
Simulating a multilevel regression model with sim.reg.nested, more specifically longitudinal data with mean centered covariates.  This function outputs the entire design matrix, individual random effects, the error, the cross product, total random effect contribution, simulated data, within cluster ID variable, and cluster level ID variable.


```r
library(simReg)
fixed <- ~1 + time + diff + act + time:act + diff:act
random <- ~1 + time
fixed.param <- c(4, 2, 6, 2.3, 7, 3)
random.param <- c(7, 4)
w.var <- 3
cov.param <- list(c(0, 1.5), c(0, 4))
n <- 50
p <- 6
errorVar <- 4
randCor <- 0
rand.dist <- "norm"
err.dist <- "norm"
serCor <- "ID"
serCorVal <- NULL
data.str <- "long"
head(temp.long <- sim.reg.nested(fixed, random, fixed.param, random.param, w.var, 
    cov.param, n, p, errorVar, randCor, rand.dist, err.dist, serCor, serCorVal, 
    data.str), n = 15)
```

```
##    X.Intercept. time     diff   act time.act diff.act     b0    b1   Fbeta
## 1             1    0  0.36781 2.544    0.000  0.93581  1.110 1.430  14.866
## 2             1    1 -0.17112 2.544    2.544 -0.43538  1.110 1.430  27.329
## 3             1    2  1.41949 2.544    5.089  3.61160  1.110 1.430  68.824
## 4             1    3 -0.94531 2.544    7.633 -2.40514  1.110 1.430  56.395
## 5             1    4  2.65848 2.544   10.177  6.76397  1.110 1.430 125.335
## 6             1    5  0.87715 2.544   12.721  2.23173  1.110 1.430 120.860
## 7             1    0 -1.23556 5.028    0.000 -6.21206 -1.789 2.859 -10.486
## 8             1    1  1.45998 5.028    5.028  7.34033 -1.789 2.859  83.539
## 9             1    2  2.36376 5.028   10.055 11.88432 -1.789 2.859 139.787
## 10            1    3 -0.25833 5.028   15.083 -1.29878 -1.789 2.859 121.699
## 11            1    4  0.44653 5.028   20.111  2.24504 -1.789 2.859 173.754
## 12            1    5 -0.20390 5.028   25.139 -1.02515 -1.789 2.859 197.235
## 13            1    0 -0.44243 1.185    0.000 -0.52417  2.599 2.772   2.498
## 14            1    1 -3.66140 1.185    1.185 -4.33790  2.599 2.772 -17.964
## 15            1    2  0.01749 1.185    2.370  0.02072  2.599 2.772  27.479
##    randEff      err sim.data withinID clustID
## 1    1.110 -2.66499   13.311        1       1
## 2    2.540 -1.31737   28.552        2       1
## 3    3.970  0.64700   73.441        3       1
## 4    5.400  0.11199   61.907        4       1
## 5    6.830 -1.05312  131.112        5       1
## 6    8.260 -1.64974  127.470        6       1
## 7   -1.789 -2.06291  -14.338        1       2
## 8    1.069 -0.01141   84.596        2       2
## 9    3.928 -4.45988  139.255        3       2
## 10   6.786 -1.51732  126.969        4       2
## 11   9.645 -1.57809  181.821        5       2
## 12  12.504 -3.22255  206.516        6       2
## 13   2.599  0.41445    5.511        1       3
## 14   5.371  1.54595  -11.046        2       3
## 15   8.144  0.90368   36.526        3       3
```

```r

## fitting lmer model
library(lme4)
lmer(sim.data ~ 1 + time + diff + act + time:act + diff:act + (1 + time | clustID), 
    data = temp.long)
```

```
## Linear mixed model fit by REML 
## Formula: sim.data ~ 1 + time + diff + act + time:act + diff:act + (1 +      time | clustID) 
##    Data: temp.long 
##   AIC  BIC logLik deviance REMLdev
##  1544 1581   -762     1509    1524
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr  
##  clustID  (Intercept) 6.59     2.57           
##           time        2.91     1.71     0.333 
##  Residual             3.82     1.96           
## Number of obs: 300, groups: clustID, 50
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   4.3778     0.4192    10.4
## time          2.0146     0.2521     8.0
## diff          6.0689     0.0941    64.5
## act           2.2476     0.1255    17.9
## time:act      7.1901     0.0756    95.1
## diff:act      3.0286     0.0323    93.7
## 
## Correlation of Fixed Effects:
##          (Intr) time   diff   act    tim:ct
## time      0.173                            
## diff      0.026 -0.003                     
## act       0.119  0.021 -0.068              
## time:act  0.022  0.120  0.023  0.174       
## diff:act -0.084  0.027 -0.045 -0.009  0.001
```


Cross Sectional Data
----------------------

```r
library(simReg)
fixed <- ~1 + numCourse + act + numCourse:act
random <- ~1
fixed.param <- c(10, 3, 6, 5)
random.param <- c(6)
w.var <- 3
cov.param <- list(c(0, 1.5), c(0, 4))
n <- 60
p <- 18
errorVar <- 4
randCor <- 0
rand.dist <- "norm"
err.dist <- "norm"
serCor <- "ID"
serCorVal <- NULL
data.str <- "cross"
head(temp.cross <- sim.reg.nested(fixed, random, fixed.param, random.param, 
    w.var, cov.param, n, p, errorVar, randCor, rand.dist, err.dist, serCor, 
    serCorVal, data.str), n = 15)
```

```
##    X.Intercept. numCourse     act numCourse.act     b0   Fbeta randEff
## 1             1    0.3146  0.2891       0.09095 -4.801  13.133  -4.801
## 2             1   -0.9305  0.2053      -0.19107 -4.801   7.485  -4.801
## 3             1   -0.5190 -0.1348       0.06995 -4.801   7.984  -4.801
## 4             1    2.2487  2.4181       5.43773 -4.801  58.444  -4.801
## 5             1    1.1207  7.2590       8.13529 -4.801  97.593  -4.801
## 6             1    0.8262 -3.9439      -3.25865 -4.801 -27.478  -4.801
## 7             1    0.7414  0.7364       0.54597 -4.801  19.372  -4.801
## 8             1   -0.8139 -1.8087       1.47208 -4.801   4.067  -4.801
## 9             1    0.4966 -0.9242      -0.45897 -4.801   3.650  -4.801
## 10            1    2.4384 -3.4220      -8.34425 -4.801 -44.938  -4.801
## 11            1    0.7883 -0.7328      -0.57770 -4.801   5.079  -4.801
## 12            1    1.3117 -1.2632      -1.65688 -4.801  -1.928  -4.801
## 13            1   -3.3268 -0.5521       1.83687 -4.801   5.891  -4.801
## 14            1   -1.0350  0.2984      -0.30881 -4.801   7.141  -4.801
## 15            1   -0.5550  2.6409      -1.46559 -4.801  16.853  -4.801
##        err sim.data withinID clustID
## 1   1.0598   9.3919        1       1
## 2  -1.2557   1.4282        2       1
## 3   1.2815   4.4646        3       1
## 4  -1.5790  52.0635        4       1
## 5  -4.2568  88.5350        5       1
## 6   0.8829 -31.3962        6       1
## 7  -0.1582  14.4132        7       1
## 8   2.2434   1.5091        8       1
## 9   0.2977  -0.8534        9       1
## 10  2.2923 -47.4466       10       1
## 11  2.7388   3.0171       11       1
## 12 -1.6165  -8.3460       12       1
## 13 -4.3725  -3.2823       13       1
## 14 -2.0566   0.2836       14       1
## 15  2.1579  14.2096       15       1
```

```r

## fitting lmer model
library(lme4)
lmer(sim.data ~ 1 + numCourse + act + numCourse:act + (1 | clustID), data = temp.cross)
```

```
## Linear mixed model fit by REML 
## Formula: sim.data ~ 1 + numCourse + act + numCourse:act + (1 | clustID) 
##    Data: temp.cross 
##   AIC  BIC logLik deviance REMLdev
##  4805 4835  -2397     4775    4793
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  clustID  (Intercept) 7.67     2.77    
##  Residual             4.01     2.00    
## Number of obs: 1080, groups: clustID, 60
## 
## Fixed effects:
##               Estimate Std. Error t value
## (Intercept)     9.9900     0.3628      28
## numCourse       2.9982     0.0414      72
## act             6.0067     0.0157     382
## numCourse:act   4.9955     0.0100     499
## 
## Correlation of Fixed Effects:
##             (Intr) numCrs act   
## numCourse   -0.007              
## act          0.005  0.002       
## numCours:ct  0.002  0.019 -0.129
```


Using sim.reg
================

Single Level
--------------


```r
library(simReg)
fixed <- ~1 + act + diff + numCourse + act:numCourse
fixed.param <- c(2, 4, 1, 3.5, 2)
cov.param <- list(c(0, 4), c(0, 3), c(0, 3))
n <- 150
errorVar <- 3
err.dist <- "norm"
head(temp.single <- sim.reg(fixed = fixed, fixed.param = fixed.param, cov.param = cov.param, 
    n = n, errorVar = errorVar, err.dist = err.dist, data.str = "single"), n = 15)
```

```
##    X.Intercept.     act    diff numCourse act.numCourse   Fbeta     err
## 1             1  2.1439  4.2985   -1.6179       -3.4686   2.274 -3.4941
## 2             1  0.6940 -4.7152   -3.8389       -2.6642 -18.704 -0.8871
## 3             1  5.6558 -3.5533   -2.7382      -15.4868 -19.487 -0.6516
## 4             1 -3.9621  4.2724    0.9600       -3.8037 -13.823  0.8836
## 5             1 -0.5152  0.2438    1.4468       -0.7454   3.756  0.2269
## 6             1  6.5381  1.4449   -1.6345      -10.6864   2.504  1.3726
## 7             1  2.0993  2.8681    1.6383        3.4392  25.878 -2.8585
## 8             1  4.4572  2.7799   -2.6025      -11.6000  -9.700  1.5197
## 9             1 -1.2804 -0.7771    2.1172       -2.7109  -1.911  0.7466
## 10            1  3.4134  0.9418    1.4859        5.0720  31.940  1.0439
## 11            1  3.3235 -0.8985    0.1517        0.5043  15.935 -1.8355
## 12            1  5.9988 -1.0248    0.4000        2.3997  31.170  0.1833
## 13            1  0.9743 -0.6389    0.8534        0.8315   9.908 -1.1380
## 14            1  0.5054 -1.7895    2.0040        1.0129  11.272 -0.7366
## 15            1 -0.1800  1.0547   -1.2673        0.2281  -1.645  0.9275
##    sim.data ID
## 1   -1.2200  1
## 2  -19.5909  2
## 3  -20.1390  3
## 4  -12.9399  4
## 5    3.9827  5
## 6    3.8764  6
## 7   23.0191  7
## 8   -8.1806  8
## 9   -1.1641  9
## 10  32.9841 10
## 11  14.0997 11
## 12  31.3532 12
## 13   8.7701 13
## 14  10.5357 14
## 15  -0.7171 15
```

```r

## Fitting regression to obtain parameter estimates
summary(lm(sim.data ~ 1 + act + diff + numCourse + act:numCourse, data = temp.single))
```

```
## 
## Call:
## lm(formula = sim.data ~ 1 + act + diff + numCourse + act:numCourse, 
##     data = temp.single)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.785 -0.938  0.131  1.040  4.261 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.08505    0.14661    14.2   <2e-16 ***
## act            3.99080    0.03266   122.2   <2e-16 ***
## diff           0.95762    0.04778    20.0   <2e-16 ***
## numCourse      3.47486    0.04526    76.8   <2e-16 ***
## act:numCourse  2.00805    0.00929   216.2   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 1.75 on 145 degrees of freedom
## Multiple R-squared: 0.998,	Adjusted R-squared: 0.998 
## F-statistic: 1.66e+04 on 4 and 145 DF,  p-value: <2e-16
```


Longitudinal
---------------

```r
library(simReg)
fixed <- ~1 + time + diff + act + time:act
random <- ~1 + time + diff
fixed.param <- c(4, 2, 6, 2.3, 7)
random.param <- c(7, 4, 2)
w.var <- 3
cov.param <- list(c(0, 1.5), c(0, 4))
n <- 150
p <- 30
errorVar <- 4
randCor <- 0
rand.dist <- "norm"
err.dist <- "norm"
serCor <- "ID"
serCorVal <- NULL
data.str <- "long"
head(temp.long <- sim.reg(fixed, random, fixed.param, random.param, w.var, cov.param, 
    n, p, errorVar, randCor, rand.dist, err.dist, serCor, serCorVal, data.str), 
    n = 15)
```

```
##    X.Intercept. time     diff    act time.act     b0     b1    b2    Fbeta
## 1             1    0  1.42884 -2.285    0.000 -1.753 -3.453 1.482    7.317
## 2             1    1 -0.30178 -2.285   -2.285 -1.753 -3.453 1.482  -17.063
## 3             1    2 -0.06583 -2.285   -4.570 -1.753 -3.453 1.482  -29.644
## 4             1    3 -0.29543 -2.285   -6.856 -1.753 -3.453 1.482  -45.018
## 5             1    4 -0.09260 -2.285   -9.141 -1.753 -3.453 1.482  -57.797
## 6             1    5 -1.16924 -2.285  -11.426 -1.753 -3.453 1.482  -78.253
## 7             1    6 -0.52429 -2.285  -13.711 -1.753 -3.453 1.482  -88.380
## 8             1    7 -1.74350 -2.285  -15.996 -1.753 -3.453 1.482 -109.692
## 9             1    8  0.08556 -2.285  -18.282 -1.753 -3.453 1.482 -112.714
## 10            1    9 -0.29263 -2.285  -20.567 -1.753 -3.453 1.482 -128.979
## 11            1   10  0.46635 -2.285  -22.852 -1.753 -3.453 1.482 -138.422
## 12            1   11 -0.13132 -2.285  -25.137 -1.753 -3.453 1.482 -156.004
## 13            1   12 -1.14315 -2.285  -27.422 -1.753 -3.453 1.482 -176.071
## 14            1   13  3.20324 -2.285  -29.708 -1.753 -3.453 1.482 -163.989
## 15            1   14 -0.18700 -2.285  -31.993 -1.753 -3.453 1.482 -198.327
##     randEff      err sim.data withinID clustID
## 1    0.3646 -1.73472    5.947        1       1
## 2   -5.6532  0.18424  -22.532        2       1
## 3   -8.7571  0.17378  -38.227        3       1
## 4  -12.5508  0.17016  -57.398        4       1
## 5  -15.7037 -0.37502  -73.876        5       1
## 6  -20.7526  1.70110  -97.305        6       1
## 7  -23.2504 -0.35736 -111.988        7       1
## 8  -28.5105  1.87220 -136.330        8       1
## 9  -29.2538  0.01673 -141.951        9       1
## 10 -33.2676  2.75238 -159.494       10       1
## 11 -35.5965 -0.49466 -174.513       11       1
## 12 -39.9356  0.38822 -195.552       12       1
## 13 -44.8884 -2.31224 -223.272       13       1
## 14 -41.9016  0.09170 -205.799       14       1
## 15 -50.3786 -0.13999 -248.846       15       1
```

```r

## fitting lmer model
library(lme4)
lmer(sim.data ~ 1 + time + diff + act + time:act + (1 + time + diff | clustID), 
    data = temp.long)
```

```
## Linear mixed model fit by REML 
## Formula: sim.data ~ 1 + time + diff + act + time:act + (1 + time + diff |      clustID) 
##    Data: temp.long 
##    AIC   BIC logLik deviance REMLdev
##  21443 21520 -10710    21406   21419
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr          
##  clustID  (Intercept) 7.33     2.71                   
##           time        4.72     2.17      0.073        
##           diff        1.73     1.31     -0.124 -0.099 
##  Residual             4.12     2.03                   
## Number of obs: 4500, groups: clustID, 150
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   4.1237     0.2295    18.0
## time          2.1890     0.1778    12.3
## diff          6.0434     0.1094    55.2
## act           2.2096     0.0552    40.1
## time:act      7.0322     0.0428   164.2
## 
## Correlation of Fixed Effects:
##          (Intr) time   diff   act   
## time      0.066                     
## diff     -0.117 -0.097              
## act      -0.063 -0.003  0.000       
## time:act -0.003 -0.063  0.000  0.055
```

