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
##    X.Intercept.     act    diff numCourse act.numCourse   Fbeta     err
## 1             1  4.8774  3.1690   1.88801       9.20852  49.704 -2.4663
## 2             1  0.5390 -1.4485   4.09838       2.20910  21.470 -2.5669
## 3             1 -3.8557  0.0835  -2.59588      10.00894  -2.407  0.9303
## 4             1  1.1829  4.4099   1.10243       1.30406  17.608  1.4568
## 5             1  5.6327  1.8039  -0.80634      -4.54190  14.429  0.2921
## 6             1  1.9112 -4.7807  -5.26820     -10.06845 -33.712 -0.6404
## 7             1 -9.2381 -1.2137  -0.25348       2.34169 -32.370  1.8221
## 8             1  0.8507  0.0830   1.57898       1.34318  13.698 -2.9000
## 9             1 -2.0353 -0.8932  -0.01469       0.02991  -7.026  0.4903
## 10            1  2.6950  1.6462  -3.25510      -8.77261 -14.512 -2.3756
## 11            1  3.5230 -0.3519  -0.07087      -0.24967  14.993  1.8928
## 12            1 -3.9580 -4.3293  -0.89369       3.53719 -14.215  2.0053
## 13            1  3.8802 -2.2765  -1.11219      -4.31551   2.721 -1.8015
## 14            1  1.9369 -1.4457   1.71279       3.31744  20.931 -1.7838
## 15            1  0.3913  1.1497   2.12987       0.83346  13.836 -1.4256
##    sim.data ID
## 1    47.237  1
## 2    18.903  2
## 3    -1.477  3
## 4    19.065  4
## 5    14.721  5
## 6   -34.352  6
## 7   -30.548  7
## 8    10.798  8
## 9    -6.536  9
## 10  -16.887 10
## 11   16.885 11
## 12  -12.209 12
## 13    0.919 13
## 14   19.148 14
## 15   12.411 15
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
## -4.042 -1.137  0.087  1.082  4.020 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     1.9151     0.1379    13.9   <2e-16 ***
## act             4.0248     0.0328   122.6   <2e-16 ***
## diff            1.0387     0.0447    23.2   <2e-16 ***
## numCourse       3.4500     0.0505    68.2   <2e-16 ***
## act:numCourse   1.9952     0.0121   164.4   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 1.67 on 145 degrees of freedom
## Multiple R-squared: 0.997,	Adjusted R-squared: 0.997 
## F-statistic: 1.14e+04 on 4 and 145 DF,  p-value: <2e-16
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
##    X.Intercept. time     diff     act time.act diff.act     b0      b1
## 1             1    0 -1.85373  4.6221   0.0000  -8.5682 -4.028  0.8698
## 2             1    1 -1.58562  4.6221   4.6221  -7.3289 -4.028  0.8698
## 3             1    2 -0.50780  4.6221   9.2442  -2.3471 -4.028  0.8698
## 4             1    3  2.23586  4.6221  13.8664  10.3344 -4.028  0.8698
## 5             1    4  0.34316  4.6221  18.4885   1.5861 -4.028  0.8698
## 6             1    5  0.07005  4.6221  23.1106   0.3238 -4.028  0.8698
## 7             1    0 -1.89402  0.7066   0.0000  -1.3383 -4.896  1.6213
## 8             1    1 -0.32082  0.7066   0.7066  -0.2267 -4.896  1.6213
## 9             1    2 -0.62057  0.7066   1.4132  -0.4385 -4.896  1.6213
## 10            1    3  0.45700  0.7066   2.1198   0.3229 -4.896  1.6213
## 11            1    4  1.11254  0.7066   2.8263   0.7861 -4.896  1.6213
## 12            1    5 -0.31159  0.7066   3.5329  -0.2202 -4.896  1.6213
## 13            1    0  1.89879 -1.2284   0.0000  -2.3324 -3.226 -2.5958
## 14            1    1 -1.19343 -1.2284  -1.2284   1.4660 -3.226 -2.5958
## 15            1    2  0.40581 -1.2284  -2.4568  -0.4985 -3.226 -2.5958
##      Fbeta  randEff     err sim.data withinID clustID
## 1  -22.196 -4.02835 -1.9146  -28.139        1       1
## 2   17.485 -3.15852 -1.3476   12.979        2       1
## 3   73.252 -2.28869  2.2582   73.222        3       1
## 4  162.114 -1.41885 -1.4596  159.236        4       1
## 5  158.868 -0.54902 -0.4481  157.871        5       1
## 6  187.797  0.32081  0.9541  189.072        6       1
## 7   -9.754 -4.89607  1.9066  -12.743        1       2
## 8    9.966 -3.27476  0.4874    7.179        2       2
## 9   14.478 -1.65346  1.7506   14.576        3       2
## 10  30.174 -0.03215 -0.3521   29.790        4       2
## 11  42.443  1.58915 -0.1904   43.842        5       2
## 12  37.826  3.21046  1.5467   42.583        6       2
## 13   5.570 -3.22579  0.3377    2.682        1       3
## 14  -8.187 -5.82159  0.5204  -13.488        2       3
## 15 -11.083 -8.41740 -0.8175  -20.318        3       3
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
##  1569 1606   -774     1533    1549
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr  
##  clustID  (Intercept) 4.45     2.11           
##           time        3.77     1.94     0.174 
##  Residual             4.28     2.07           
## Number of obs: 300, groups: clustID, 50
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   4.2272     0.3666    11.5
## time          2.2317     0.2835     7.9
## diff          5.9409     0.0901    66.0
## act           2.2745     0.0996    22.8
## time:act      7.0482     0.0771    91.4
## diff:act      2.9622     0.0262   113.0
## 
## Correlation of Fixed Effects:
##          (Intr) time   diff   act    tim:ct
## time      0.019                            
## diff      0.057 -0.016                     
## act       0.027  0.001 -0.001              
## time:act  0.001  0.027  0.006  0.020       
## diff:act  0.002  0.006  0.082  0.004 -0.004
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
##    X.Intercept. numCourse     act numCourse.act    b0    Fbeta randEff
## 1             1   -0.6690  4.0788       -2.7289 2.716   18.822   2.716
## 2             1    0.9564  0.2637        0.2522 2.716   15.712   2.716
## 3             1   -1.9810 -0.7228        1.4318 2.716    6.879   2.716
## 4             1    0.4972 -0.8426       -0.4189 2.716    4.342   2.716
## 5             1    0.1302  3.2595        0.4243 2.716   32.069   2.716
## 6             1   -1.5921  0.3378       -0.5379 2.716    4.562   2.716
## 7             1    1.3739 -0.1447       -0.1988 2.716   12.260   2.716
## 8             1   -1.1657  3.8053       -4.4360 2.716    7.155   2.716
## 9             1    2.2795  3.3179        7.5633 2.716   74.562   2.716
## 10            1    2.4110 -4.6395      -11.1856 2.716  -66.532   2.716
## 11            1   -2.0731  3.3979       -7.0440 2.716  -11.052   2.716
## 12            1    3.4442 -5.3944      -18.5796 2.716 -104.932   2.716
## 13            1   -0.7051  2.9439       -2.0757 2.716   15.170   2.716
## 14            1    0.3250  0.6050        0.1966 2.716   15.588   2.716
## 15            1    3.0190 -2.9676       -8.9591 2.716  -43.544   2.716
##        err sim.data withinID clustID
## 1  -2.0584   19.479        1       1
## 2  -2.4258   16.002        2       1
## 3  -1.2291    8.367        3       1
## 4  -1.0309    6.027        4       1
## 5   0.6779   35.463        5       1
## 6  -1.1458    6.132        6       1
## 7  -0.4939   14.482        7       1
## 8  -0.2415    9.630        8       1
## 9   1.3894   78.668        9       1
## 10  3.7035  -60.113       10       1
## 11 -1.0088   -9.345       11       1
## 12  2.0457 -100.170       12       1
## 13 -2.0498   15.836       13       1
## 14  1.7808   20.085       14       1
## 15  0.1476  -40.680       15       1
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
##  4791 4821  -2389     4760    4779
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  clustID  (Intercept) 6.40     2.53    
##  Residual             3.99     2.00    
## Number of obs: 1080, groups: clustID, 60
## 
## Fixed effects:
##               Estimate Std. Error t value
## (Intercept)    10.0723     0.3323      30
## numCourse       3.0238     0.0408      74
## act             6.0005     0.0159     378
## numCourse:act   4.9878     0.0108     462
## 
## Correlation of Fixed Effects:
##             (Intr) numCrs act   
## numCourse   -0.004              
## act          0.001  0.003       
## numCours:ct  0.002  0.030  0.023
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
##    X.Intercept.     act     diff numCourse act.numCourse   Fbeta     err
## 1             1 -0.7153 -6.68891  -3.54791        2.5379 -14.892 -1.4647
## 2             1 -1.5846  2.88037   0.93123       -1.4756  -1.150 -2.9124
## 3             1 -2.1673 -2.28729   3.94544       -8.5508 -12.249 -1.4272
## 4             1 -4.1883 -0.08983  -5.98023       25.0468  14.320 -0.9441
## 5             1  0.4111  3.78211  -0.96563       -0.3970   3.253  1.4159
## 6             1  0.2085 -4.59283   0.50083        0.1044   0.203 -0.6860
## 7             1 -3.6071 -1.69374  -2.36767        8.5405  -5.328 -1.3875
## 8             1  0.5263  1.32156  -0.31158       -0.1640   4.008  0.4723
## 9             1  2.7775  3.22426  -3.61007      -10.0269 -16.355 -1.2475
## 10            1  1.5025  1.88063   0.12665        0.1903  10.715 -1.3303
## 11            1 -3.3285 -2.10308   2.19538       -7.3074 -20.348 -2.8231
## 12            1  6.3740  0.32519   1.21557        7.7480  47.572 -1.3520
## 13            1 -6.9555  1.66969   2.51821      -17.5155 -50.370  0.9965
## 14            1 -3.0182 -2.49045   1.57435       -4.7517 -16.556  2.3217
## 15            1 -1.5986  1.58996   0.07421       -0.1186  -2.782 -2.3358
##    sim.data ID
## 1   -16.357  1
## 2    -4.062  2
## 3   -13.676  3
## 4    13.376  4
## 5     4.669  5
## 6    -0.483  6
## 7    -6.716  7
## 8     4.481  8
## 9   -17.602  9
## 10    9.384 10
## 11  -23.171 11
## 12   46.220 12
## 13  -49.373 13
## 14  -14.234 14
## 15   -5.118 15
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
## -4.011 -1.170  0.088  1.171  3.099 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     1.9199     0.1336    14.4   <2e-16 ***
## act             4.0825     0.0336   121.3   <2e-16 ***
## diff            0.9892     0.0469    21.1   <2e-16 ***
## numCourse       3.4329     0.0480    71.6   <2e-16 ***
## act:numCourse   2.0152     0.0118   170.3   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 1.61 on 145 degrees of freedom
## Multiple R-squared: 0.997,	Adjusted R-squared: 0.997 
## F-statistic: 1.33e+04 on 4 and 145 DF,  p-value: <2e-16
```



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
##    X.Intercept. time     diff   act time.act    b0    b1      b2  Fbeta
## 1             1    0  1.88642 2.404    0.000 4.942 1.129 -0.2579  20.85
## 2             1    1  1.90829 2.404    2.404 4.942 1.129 -0.2579  39.80
## 3             1    2 -0.66613 2.404    4.807 4.942 1.129 -0.2579  43.18
## 4             1    3  0.70124 2.404    7.211 4.942 1.129 -0.2579  70.21
## 5             1    4  0.90149 2.404    9.615 4.942 1.129 -0.2579  90.24
## 6             1    5 -2.52901 2.404   12.019 4.942 1.129 -0.2579  88.48
## 7             1    6  0.38604 2.404   14.422 4.942 1.129 -0.2579 124.80
## 8             1    7  1.03225 2.404   16.826 4.942 1.129 -0.2579 147.50
## 9             1    8  0.01438 2.404   19.230 4.942 1.129 -0.2579 160.22
## 10            1    9 -0.27165 2.404   21.633 4.942 1.129 -0.2579 177.33
## 11            1   10  1.69015 2.404   24.037 4.942 1.129 -0.2579 207.93
## 12            1   11  1.77227 2.404   26.441 4.942 1.129 -0.2579 227.25
## 13            1   12  1.62146 2.404   28.845 4.942 1.129 -0.2579 245.17
## 14            1   13 -0.46457 2.404   31.248 4.942 1.129 -0.2579 251.48
## 15            1   14  0.21232 2.404   33.652 4.942 1.129 -0.2579 274.37
##    randEff     err sim.data withinID clustID
## 1    4.455 -1.6809    23.62        1       1
## 2    5.578  0.5647    45.95        2       1
## 3    7.371 -3.0092    47.55        3       1
## 4    8.147 -2.6765    75.68        4       1
## 5    9.224 -4.9772    94.49        5       1
## 6   11.237  0.4123   100.13        6       1
## 7   11.614  1.8064   138.22        7       1
## 8   12.576 -0.4832   159.60        8       1
## 9   13.967  1.8764   176.07        9       1
## 10  15.169  0.1515   192.65       10       1
## 11  15.792 -0.8524   222.87       11       1
## 12  16.899 -1.6736   242.47       12       1
## 13  18.067 -0.5197   262.72       13       1
## 14  19.733 -3.4311   267.78       14       1
## 15  20.687  2.4709   297.53       15       1
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
##  21218 21295 -10597    21179   21194
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr          
##  clustID  (Intercept) 6.31     2.51                   
##           time        3.52     1.88     -0.002        
##           diff        1.85     1.36      0.038 -0.068 
##  Residual             3.95     1.99                   
## Number of obs: 4500, groups: clustID, 150
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   3.9993     0.2134    18.7
## time          2.0181     0.1534    13.2
## diff          5.9053     0.1132    52.2
## act           2.3147     0.0550    42.1
## time:act      7.0813     0.0394   179.6
## 
## Correlation of Fixed Effects:
##          (Intr) time   diff   act   
## time     -0.007                     
## diff      0.035 -0.067              
## act      -0.029  0.000  0.000       
## time:act  0.000 -0.028  0.000 -0.005
```

