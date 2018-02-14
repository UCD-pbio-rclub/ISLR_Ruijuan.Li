---
title: "chapter_5_2"
author: "Ruijuan Li"
date: "2/13/2018"
output: 
  html_document: 
    keep_md: yes
---

For this part, refer to Julin's work 
https://github.com/UCD-pbio-rclub/ISLR_Julin.Maloof/blob/master/Chapter_5/Chapter5Problems.md 

2. We will now derive the probability that a given observation is part of a bootstrap sample. Suppose that we obtain a bootstrap sample from a set of n observations.

(a) What is the probability that the first bootstrap observation is not the jth observation from the original sample? Justify your answer.

n-1/n 

(b) What is the probability that the second bootstrap observation is not the jth observation from the original sample?

n-1/n

(c) Argue that the probability that the jth observation is not in the bootstrap sample is (1 − 1/n)n.

We have n picks for the bootstrap. The probability that any one of them is not the jth sample is (1 - 1/n). We just multiply them to get the total probability. 

(d) When n = 5, what is the probability that the jth observation is in the bootstrap sample?

(e) When n = 100, what is the probability that the jth observation is in the bootstrap sample?

(f) When n = 10, 000, what is the probability that the jth observa- tion is in the bootstrap sample?


```r
1-(4/5)^5 
```

```
## [1] 0.67232
```

```r
1-(99/100)^100
```

```
## [1] 0.6339677
```

```r
1-(9999/10000)^10000 
```

```
## [1] 0.632139
```


(g) Create a plot that displays, for each integer value of n from 1 to 100,000, the probability that the jth observation is in the bootstrap sample. Comment on what you observe.

(h) We will now investigate numerically the probability that a boot- strap sample of size n = 100 contains the jth observation. Here j = 4. We repeatedly create bootstrap samples, and each time we record whether or not the fourth observation is contained in the bootstrap sample.


```r
store=rep(NA, 10000) 

for(i in 1:10000){
store[i]=sum(sample(1:100, rep=TRUE)==4)>0 }
mean(store)
```

```
## [1] 0.6335
```

Comment on the results obtained.

4. Suppose that we use some statistical learning method to make a pre- diction for the response Y for a particular value of the predictor X. Carefully describe how we might estimate the standard deviation of our prediction.

randomly draw samples with replacement and fit the same model as how we do this for the whole dataset, then draw another set of samples, repeat the analysis, then we can get the stdv from this bootstrap method 

6. We continue to consider the use of a logistic regression model to predict the probability of default using income and balance on the Default data set. In particular, we will now compute estimates for the standard errors of the income and balance logistic regression co- efficients in two different ways: (1) using the bootstrap, and (2) using the standard formula for computing the standard errors in the glm() function. Do not forget to set a random seed before beginning your analysis.

(a) Using the summary() and glm() functions, determine the esti- mated standard errors for the coefficients associated with income and balance in a multiple logistic regression model that uses both predictors.

(b) Write a function, boot.fn(), that takes as input the Default data set as well as an index of the observations, and that outputs the coefficient estimates for income and balance in the multiple logistic regression model.

(c) Use the boot() function together with your boot.fn() function to estimate the standard errors of the logistic regression coefficients for income and balance.

(d) Comment on the estimated standard errors obtained using the glm() function and using your bootstrap function. 


```r
library(tidyverse)
```

```
## ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
## ✔ tibble  1.3.4     ✔ dplyr   0.7.4
## ✔ tidyr   0.7.2     ✔ stringr 1.2.0
## ✔ readr   1.1.1     ✔ forcats 0.2.0
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```
## Warning: package 'tibble' was built under R version 3.2.5
```

```
## Warning: package 'tidyr' was built under R version 3.2.5
```

```
## Warning: package 'readr' was built under R version 3.2.5
```

```
## Warning: package 'purrr' was built under R version 3.2.5
```

```
## Warning: package 'dplyr' was built under R version 3.2.5
```

```
## Warning: package 'stringr' was built under R version 3.2.5
```

```
## Warning: package 'forcats' was built under R version 3.2.5
```

```
## ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(MASS)
```

```
## Warning: package 'MASS' was built under R version 3.2.5
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
library(ISLR)
```

```
## Warning: package 'ISLR' was built under R version 3.2.5
```

```r
library(boot)
```

```
## Warning: package 'boot' was built under R version 3.2.5
```

```r
data("Default")
Default %>% colnames()
```

```
## [1] "default" "student" "balance" "income"
```

```r
# a) 
glm1 <- glm(default ~ balance + income, family=binomial, data=Default)
summary(glm1)
```

```
## 
## Call:
## glm(formula = default ~ balance + income, family = binomial, 
##     data = Default)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4725  -0.1444  -0.0574  -0.0211   3.7245  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.154e+01  4.348e-01 -26.545  < 2e-16 ***
## balance      5.647e-03  2.274e-04  24.836  < 2e-16 ***
## income       2.081e-05  4.985e-06   4.174 2.99e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2920.6  on 9999  degrees of freedom
## Residual deviance: 1579.0  on 9997  degrees of freedom
## AIC: 1585
## 
## Number of Fisher Scoring iterations: 8
```

```r
# b) 
boot.fn=function(data,index){
return(coef(glm(default~balance + income ,data=data,subset=index, family=binomial))) 
}

# c) 
set.seed(1)
boot(Default ,boot.fn ,1000)
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Default, statistic = boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##          original        bias     std. error
## t1* -1.154047e+01 -8.008379e-03 4.239273e-01
## t2*  5.647103e-03  2.299970e-06 2.267955e-04
## t3*  2.080898e-05  5.870933e-08 4.582525e-06
```

```r
# d) 
# the two sets of values are very similar to each other  
```

9. We will now consider the Boston housing data set, from the MASS library.

(a) Based on this data set, provide an estimate for the population mean of medv. Call this estimate μˆ.

(b) Provide an estimate of the standard error of μˆ. Interpret this result.
Hint: We can compute the standard error of the sample mean by dividing the sample standard deviation by the square root of the number of observations.

(c) Now estimate the standard error of μˆ using the bootstrap. How does this compare to your answer from (b)?

(d) Based on your bootstrap estimate from (c), provide a 95 % con- fidence interval for the mean of medv. Compare it to the results obtained using t.test(Boston$medv).
Hint: You can approximate a 95 % confidence interval using the formula [μˆ − 2SE(μˆ), μˆ + 2SE(μˆ)].

(e) Based on this data set, provide an estimate, μˆmed, for the median value of medv in the population.

(f) Wenowwouldliketoestimatethestandarderrorofμˆmed.Unfor- tunately, there is no simple formula for computing the standard error of the median. Instead, estimate the standard error of the median using the bootstrap. Comment on your findings.

(g) Based on this data set, provide an estimate for the tenth per- centile of medv in Boston suburbs. Call this quantity μˆ0.1. (You can use the quantile() function.)

(h) Use the bootstrap to estimate the standard error of μˆ0.1. Com- ment on your findings.


```r
# a) 
mu_hat <- mean(Boston$medv)
mu_hat
```

```
## [1] 22.53281
```

```r
# b) 
sd(Boston$medv)/sqrt(nrow(Boston))
```

```
## [1] 0.4088611
```

```r
# c) 
set.seed(1)
boot.fn <- function(data,index) {
  mean(data[index])
}

(boot.result <- boot(Boston$medv,boot.fn,1000)) 
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Boston$medv, statistic = boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##     original      bias    std. error
## t1* 22.53281 0.008517589   0.4119374
```

```r
# what is the index used in boot() ??? actually the number of samples in bootstrap sample is always the same as the original dataset 

?boot() # 

# d) 
t.test(Boston$medv)
```

```
## 
## 	One Sample t-test
## 
## data:  Boston$medv
## t = 55.111, df = 505, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  21.72953 23.33608
## sample estimates:
## mean of x 
##  22.53281
```

```r
sd.boot <- sd(boot.result$t) # the way to get stdv from boot result 
mean(Boston$medv - 2*sd.boot) 
```

```
## [1] 21.70893
```

```r
mean(Boston$medv + 2*sd.boot) 
```

```
## [1] 23.35668
```

```r
# e) 
median(Boston$medv)
```

```
## [1] 21.2
```

```r
# f) 
boot.fn <- function(data,index) {
  median(data[index])
}
(boot.result <- boot(Boston$medv,boot.fn,1000))
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Boston$medv, statistic = boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##     original  bias    std. error
## t1*     21.2 -0.0098   0.3874004
```

```r
# g) 
boot.fn <- function(data,index) {
  quantile(data[index],.1)
}
(boot.result <- boot(Boston$medv,boot.fn,1000)) 
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Boston$medv, statistic = boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##     original  bias    std. error
## t1*    12.75 0.00515   0.5113487
```


