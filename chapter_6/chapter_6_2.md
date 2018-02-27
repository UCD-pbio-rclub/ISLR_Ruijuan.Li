---
title: "chapter_6_2"
author: "Ruijuan Li"
date: "2/26/2018"
output: 
  html_document: 
    keep_md: yes
---

### 3

a) think this as the RSS for linear regression, as s is increased from 0, more and more predictors can be added to the model, so training error will decrease steadily and flatten out. 

b) test RSS will decrease and then flattern out almost, as training RSS. 

c) variance: Variance refers to your algorithm's sensitivity to specific sets of training data. is an error from sensitivity to small fluctuations in the training set. as s is increased from 0, more predictors are added to the model, variance will increase (sensitivity increase --> overfitting)   

d) (squared) bias: Bias is the difference between your model's expected predictions and the true values. The bias is an error from erroneous assumptions in the learning algorithm. High bias can cause an algorithm to miss the relevant relations between features and target outputs (underfitting). As more and more predictors added, bias will decrease (generalization decrease --> underfitting) 

e) irreducible error: is also known as "noise," and it can't be reduced by your choice in algorithm. It typically comes from inherent randomness, a mis-framed problem, or an incomplete feature set. so it remaines constant.  

### 4 

This is the case for ridge regression 

a) training RSS. as lambda is increasing, the impact of the shrinkage penalty grows, and the ridge regression coefficient estimates will approach zero, training RSS will increase and then flatten out (as the model is becoming less and less sensitive to the training data, less and less flexible) 

b) test RSS will decrease (as model is becoming more and more generalized to fit on the test dataset) and then increase (as many predictors are shrinked to have coefficient of zero), form a U shape.  

c) variance will decrease as lamba increases, because the sensitivity decreases. 

d) bias will increase because the model is more generalized. 

e) irreducible error doesn't change 

### 5 

a) (y1 − β1x11 − β2x12)^2 + (y2 − β1x21 − β2x22)^2 + λ(β1^2 + β2^2).

b) see https://www.mathstat.dal.ca/~aarms2014/StatLearn/assignments/A3sol_2.pdf 

### 9 


```r
library(tidyverse)
```

```
## ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
## ✔ readr   1.1.1     ✔ forcats 0.3.0
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
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
## ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(ISLR)
```

```
## Warning: package 'ISLR' was built under R version 3.2.5
```

```r
library(leaps)
```

```
## Warning: package 'leaps' was built under R version 3.2.5
```

```r
# a) 
dim(College)
```

```
## [1] 777  18
```

```r
colnames(College)
```

```
##  [1] "Private"     "Apps"        "Accept"      "Enroll"      "Top10perc"  
##  [6] "Top25perc"   "F.Undergrad" "P.Undergrad" "Outstate"    "Room.Board" 
## [11] "Books"       "Personal"    "PhD"         "Terminal"    "S.F.Ratio"  
## [16] "perc.alumni" "Expend"      "Grad.Rate"
```

```r
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(College),rep=TRUE)
test =(! train )

# b) fit the model using least square on the training set, and report the test error obtained.
# least square (linear regression, using best subset method) 
regfit.best=regsubsets(Apps~.,data=College[train,], nvmax =17) # best subset using only traning data 
plot(regfit.best,scale="bic") # why not using this method??? 
```

![](chapter_6_2_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
test.mat<-model.matrix(Apps~.,data=College[test,]) 
dim(test.mat)
```

```
## [1] 377  18
```

```r
sum(test)
```

```
## [1] 377
```

```r
val.errors=rep(NA,17) 

for(i in 1:17){
coefi=coef(regfit.best,id=i)
pred=test.mat[,names(coefi)]%*%coefi
val.errors[i]=mean((College$Apps[test]-pred)^2) }

val.errors
```

```
##  [1] 1714544 1542316 1510924 1512552 1492114 1635683 1645549 1610020
##  [9] 1626584 1616854 1555953 1520681 1526317 1519996 1522719 1520481
## [17] 1520331
```

```r
which.min(val.errors) # 14 predictors 
```

```
## [1] 5
```

```r
val.errors[which.min(val.errors)] # test error ??? 
```

```
## [1] 1492114
```

```r
coef(regfit.best, 5)  
```

```
##   (Intercept)    PrivateYes        Accept     Top10perc     Top25perc 
## -114.81292106 -617.07313758    1.28203523   50.65442163  -16.79622599 
##        Expend 
##    0.05489076
```

```r
# c) Fit a ridge regression model on the training set, with λ chosen by cross-validation. Report the test error obtained. 

library(glmnet) 
```

```
## Warning: package 'glmnet' was built under R version 3.2.5
```

```
## Loading required package: Matrix
```

```
## Warning: package 'Matrix' was built under R version 3.2.5
```

```
## 
## Attaching package: 'Matrix'
```

```
## The following object is masked from 'package:tidyr':
## 
##     expand
```

```
## Loading required package: foreach
```

```
## Warning: package 'foreach' was built under R version 3.2.5
```

```
## 
## Attaching package: 'foreach'
```

```
## The following objects are masked from 'package:purrr':
## 
##     accumulate, when
```

```
## Loaded glmnet 2.0-13
```

```r
### fit ridge regression on training set 
grid=10^seq(10,-2,length=100)
x = model.matrix(Apps~.,data=College[train,])
dim(x)  # 
```

```
## [1] 400  18
```

```r
y = College$Apps[train]
length(y)
```

```
## [1] 400
```

```r
ridge.mod=glmnet(x, y, alpha=0,lambda=grid, thresh =1e-12)
dim(coef(ridge.mod))
```

```
## [1]  19 100
```

```r
### cross-validation to choose lamda 

### test error on test data set 
```

