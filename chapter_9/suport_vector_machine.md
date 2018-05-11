---
title: "support_vector_machine"
author: "Ruijuan Li"
date: "5/11/2018"
output: 
  html_document: 
    keep_md: yes
---

### 4
Generate a simulated two-class data set with 100 observations and two features in which there is a visible but non-linear separation between the two classes. Show that in this setting, a support vector machine with a polynomial kernel (with degree greater than 1) or a radial kernel will outperform a support vector classifier on the training data. Which technique performs best on the test data? Make plots and report training and test error rates in order to back up your assertions.


```r
library(e1071)
```

```
## Warning: package 'e1071' was built under R version 3.2.5
```

```r
# generate data 
set.seed (1)
x=matrix(rnorm(100*2), ncol=2)
y=c(rep(-1,50), rep(1,50))
x[y==1,]=x[y==1,] + 1 # don't understand 
plot(x, col=(3-y)) # don't understand 
```

![](suport_vector_machine_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
# splitting to training and test data set 
train=sample(100,80)

# support vector classifier, use CV to decide cost value 
dat=data.frame(x=x, y=as.factor(y))

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out) # training error, 0.225 
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost
##   0.1
## 
## - best performance: 0.2125 
## 
## - Detailed performance results:
##    cost  error dispersion
## 1 1e-03 0.5125  0.1608355
## 2 1e-02 0.2250  0.1536591
## 3 1e-01 0.2125  0.1449377
## 4 1e+00 0.2250  0.1645701
## 5 5e+00 0.2250  0.1645701
## 6 1e+01 0.2250  0.1645701
## 7 1e+02 0.2250  0.1645701
```

```r
bestmod=tune.out$best.model 
summary(bestmod)
```

```
## 
## Call:
## best.tune(method = svm, train.x = y ~ ., data = dat[train, ], 
##     ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)), 
##     kernel = "linear")
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  0.1 
##       gamma:  0.5 
## 
## Number of Support Vectors:  50
## 
##  ( 25 25 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  -1 1
```

```r
plot(bestmod, dat)
```

![](suport_vector_machine_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
ypred=predict(bestmod ,dat[-train,])
table(predict=ypred, truth=dat[-train,]$y) 
```

```
##        truth
## predict -1 1
##      -1  7 3
##      1   2 8
```

```r
(2+3)/20 # 0.25 test error 
```

```
## [1] 0.25
```

```r
# support vector machine with radial kernel 
set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4) ))
summary(tune.out) # training error 0.2 
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost gamma
##   0.1   0.5
## 
## - best performance: 0.225 
## 
## - Detailed performance results:
##     cost gamma  error dispersion
## 1  1e-01   0.5 0.2250  0.1645701
## 2  1e+00   0.5 0.2625  0.1811422
## 3  1e+01   0.5 0.2750  0.1936492
## 4  1e+02   0.5 0.3000  0.2140872
## 5  1e+03   0.5 0.3125  0.1976424
## 6  1e-01   1.0 0.2250  0.1645701
## 7  1e+00   1.0 0.2875  0.1772671
## 8  1e+01   1.0 0.2875  0.2045490
## 9  1e+02   1.0 0.3125  0.2224391
## 10 1e+03   1.0 0.3375  0.1671867
## 11 1e-01   2.0 0.3625  0.1496524
## 12 1e+00   2.0 0.2875  0.1868043
## 13 1e+01   2.0 0.3250  0.1972027
## 14 1e+02   2.0 0.3000  0.1787301
## 15 1e+03   2.0 0.3750  0.1559024
## 16 1e-01   3.0 0.4875  0.1094494
## 17 1e+00   3.0 0.3000  0.1787301
## 18 1e+01   3.0 0.3375  0.2045490
## 19 1e+02   3.0 0.3250  0.1581139
## 20 1e+03   3.0 0.3375  0.1772671
## 21 1e-01   4.0 0.5375  0.1029091
## 22 1e+00   4.0 0.3000  0.1881932
## 23 1e+01   4.0 0.3000  0.1972027
## 24 1e+02   4.0 0.3125  0.1976424
## 25 1e+03   4.0 0.3375  0.1868043
```

```r
plot(tune.out$best.model,dat)
```

![](suport_vector_machine_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
table(true=dat[-train,"y"], pred=predict(tune.out$best.model, newdata=dat[-train,]))
```

```
##     pred
## true -1 1
##   -1  6 3
##   1   3 8
```

```r
(4+1)/20 # 0.25 test error, same as suppor vecotr classifier 
```

```
## [1] 0.25
```

### 5
We have seen that we can fit an SVM with a non-linear kernel in order to perform classification using a non-linear decision boundary. We will now see that we can also obtain a non-linear decision boundary by performing logistic regression using non-linear transformations of the features.

(a) Generate a data set with n = 500 and p = 2, such that the observations belong to two classes with a quadratic decision boundary between them. For instance, you can do this as follows:

```r
x1=runif(500)-0.5
x2=runif(500)-0.5
y=1*(x1^2-x2^2 > 0)
```

(b) Plot the observations, colored according to their class labels. Your plot should display X1 on the x-axis, and X2 on the y- axis.

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
dat <- data.frame(x1 = x1, 
                  x2 = x2,
                  y = as.factor(y))

dat %>% 
  ggplot() + 
  geom_point(aes(x = x1, y = x2, color = y))
```

![](suport_vector_machine_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

(c) Fit a logistic regression model to the data, using X1 and X2 as predictors.

```r
library(ISLR)
```

```
## Warning: package 'ISLR' was built under R version 3.2.5
```

```r
glm.fits=glm(y ~ ., data=dat,family=binomial)
summary(glm.fits)
```

```
## 
## Call:
## glm(formula = y ~ ., family = binomial, data = dat)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.196  -1.160  -1.133   1.194   1.229  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -0.03983    0.08949  -0.445    0.656
## x1           0.11727    0.31321   0.374    0.708
## x2          -0.06411    0.30463  -0.210    0.833
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 692.95  on 499  degrees of freedom
## Residual deviance: 692.77  on 497  degrees of freedom
## AIC: 698.77
## 
## Number of Fisher Scoring iterations: 3
```

(d) Apply this model to the training data in order to obtain a predicted class label for each training observation. Plot the observations, colored according to the predicted class labels. The decision boundary should be linear.

```r
set.seed(1)
train = sample(500, 500*0.8)
glm.fits=glm(y ~ ., data=dat[train,],family=binomial)
glm.probs=predict(glm.fits,type="response")
glm.pred <- ifelse(glm.probs > .5, 1, 0) %>% as.character() %>% as.factor() 

dat[train,] %>% 
  ggplot() + 
  geom_point(aes(x = x1, y = x2, color = glm.pred))
```

![](suport_vector_machine_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

(e) Now fit a logistic regression model to the data using non-linear functions of X1 and X2 as predictors (e.g. X12, X1 ×X2, log(X2), and so forth).

```r
library(ISLR)
glm.fits=glm(y ~ poly(x1, 2) + x1*x2, data=dat,family=binomial)
summary(glm.fits)
```

```
## 
## Call:
## glm(formula = y ~ poly(x1, 2) + x1 * x2, family = binomial, data = dat)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4169  -0.7427  -0.5938   0.7615   1.8990  
## 
## Coefficients: (1 not defined because of singularities)
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   0.07463    0.11284   0.661    0.508    
## poly(x1, 2)1  1.61592    2.69516   0.600    0.549    
## poly(x1, 2)2 34.33414    3.12900  10.973   <2e-16 ***
## x1                 NA         NA      NA       NA    
## x2           -0.04964    0.37041  -0.134    0.893    
## x1:x2        -1.26469    1.43849  -0.879    0.379    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 692.95  on 499  degrees of freedom
## Residual deviance: 505.68  on 495  degrees of freedom
## AIC: 515.68
## 
## Number of Fisher Scoring iterations: 4
```

(f) Apply this model to the training data in order to obtain a predicted class label for each training observation. Plot the observations, colored according to the predicted class labels. The decision boundary should be obviously non-linear. If it is not, then repeat (a)-(e) until you come up with an example in which the predicted class labels are obviously non-linear.

```r
glm.fits=glm(y ~ poly(x1, 2) + x1*x2, data=dat[train,],family=binomial)
glm.probs=predict(glm.fits,type="response")
glm.pred <- ifelse(glm.probs > .5, 1, 0) %>% as.character() %>% as.factor() 

dat[train,] %>% 
  ggplot() + 
  geom_point(aes(x = x1, y = x2, color = glm.pred))
```

![](suport_vector_machine_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

(g) Fit a support vector classifier to the data with X1 and X2 as predictors. Obtain a class prediction for each training observation. Plot the observations, colored according to the predicted class labels.

```r
dat=data.frame(x=x, y=as.factor(y))

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out) 
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost
##     1
## 
## - best performance: 0.525 
## 
## - Detailed performance results:
##    cost  error dispersion
## 1 1e-03 0.5425 0.07173446
## 2 1e-02 0.5450 0.06851602
## 3 1e-01 0.5350 0.06258328
## 4 1e+00 0.5250 0.06666667
## 5 5e+00 0.5300 0.06213784
## 6 1e+01 0.5300 0.06213784
## 7 1e+02 0.5275 0.06395528
```

```r
bestmod=tune.out$best.model 
summary(bestmod)
```

```
## 
## Call:
## best.tune(method = svm, train.x = y ~ ., data = dat[train, ], 
##     ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)), 
##     kernel = "linear")
## 
## 
## Parameters:
##    SVM-Type:  C-classification 
##  SVM-Kernel:  linear 
##        cost:  1 
##       gamma:  0.5 
## 
## Number of Support Vectors:  391
## 
##  ( 195 196 )
## 
## 
## Number of Classes:  2 
## 
## Levels: 
##  0 1
```

```r
plot(bestmod, dat)
```

![](suport_vector_machine_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
ypred=predict(bestmod ,dat[-train,])
table(predict=ypred, truth=dat[-train,]$y) 
```

```
##        truth
## predict  0  1
##       0 37 36
##       1 15 12
```

```r
(25+31)/100 # 0.56 test error 
```

```
## [1] 0.56
```

(h) Fit a SVM using a non-linear kernel to the data. Obtain a class prediction for each training observation. Plot the observations, colored according to the predicted class labels.

```r
set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4) ))
summary(tune.out) 
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost gamma
##     1   0.5
## 
## - best performance: 0.4325 
## 
## - Detailed performance results:
##     cost gamma  error dispersion
## 1  1e-01   0.5 0.4775 0.08616038
## 2  1e+00   0.5 0.4325 0.07458217
## 3  1e+01   0.5 0.4575 0.08337499
## 4  1e+02   0.5 0.4925 0.06775159
## 5  1e+03   0.5 0.4950 0.08644202
## 6  1e-01   1.0 0.4825 0.08583738
## 7  1e+00   1.0 0.4500 0.08498366
## 8  1e+01   1.0 0.4800 0.08316650
## 9  1e+02   1.0 0.4750 0.09204468
## 10 1e+03   1.0 0.4725 0.07212066
## 11 1e-01   2.0 0.4900 0.09944289
## 12 1e+00   2.0 0.4850 0.08349983
## 13 1e+01   2.0 0.4400 0.08432740
## 14 1e+02   2.0 0.4825 0.06241661
## 15 1e+03   2.0 0.5050 0.05244044
## 16 1e-01   3.0 0.5000 0.10408330
## 17 1e+00   3.0 0.4650 0.10013879
## 18 1e+01   3.0 0.4600 0.06892024
## 19 1e+02   3.0 0.4775 0.05945353
## 20 1e+03   3.0 0.4875 0.08436857
## 21 1e-01   4.0 0.5025 0.10765815
## 22 1e+00   4.0 0.4525 0.07402139
## 23 1e+01   4.0 0.4800 0.07799573
## 24 1e+02   4.0 0.5000 0.07546154
## 25 1e+03   4.0 0.4875 0.07569126
```

```r
plot(tune.out$best.model,dat)
```

![](suport_vector_machine_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
table(true=dat[-train,"y"], pred=predict(tune.out$best.model, newdata=dat[-train,]))
```

```
##     pred
## true  0  1
##    0 38 14
##    1 42  6
```

```r
(0+1)/100 # 0.01 test error 
```

```
## [1] 0.01
```

(i) Comment on your results. 
SVM has the best performance 

### 7
In this problem, you will use support vector approaches in order to predict whether a given car gets high or low gas mileage based on the Auto data set.

(a) Create a binary variable that takes on a 1 for cars with gas mileage above the median, and a 0 for cars with gas mileage below the median.


```r
Auto$y <- ifelse(Auto$mpg < median(Auto$mpg), 1, 0)
dim(Auto)
```

```
## [1] 392  10
```

(b) Fit a support vector classifier to the data with various values of cost, in order to predict whether a car gets high or low gas mileage. Report the cross-validation errors associated with different values of this parameter. Comment on your results.


```r
set.seed(1)
train <- sample(392, 392*0.8)
tune.out1=tune(svm,y~.,data=Auto[train,],kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out1)  # best is with 0.075
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost
##     1
## 
## - best performance: 0.07587471 
## 
## - Detailed performance results:
##    cost      error dispersion
## 1 1e-03 0.10024058 0.02179633
## 2 1e-02 0.08549502 0.02452468
## 3 1e-01 0.08046756 0.02410481
## 4 1e+00 0.07587471 0.02218601
## 5 5e+00 0.08296032 0.02451379
## 6 1e+01 0.08904159 0.02621574
## 7 1e+02 0.10433531 0.03542994
```

```r
tune.out1$best.model 
```

```
## 
## Call:
## best.tune(method = svm, train.x = y ~ ., data = Auto[train, ], 
##     ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)), 
##     kernel = "linear")
## 
## 
## Parameters:
##    SVM-Type:  eps-regression 
##  SVM-Kernel:  linear 
##        cost:  1 
##       gamma:  0.003205128 
##     epsilon:  0.1 
## 
## 
## Number of Support Vectors:  261
```

(c) Now repeat (b), this time using SVMs with radial and polynomial basis kernels, with different values of gamma and degree and cost. Comment on your results.

```r
# radial 
set.seed(1)
tune.out2=tune(svm, y~., data=Auto[train,], kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4) ))
summary(tune.out2) # 0.052
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost gamma
##     1   0.5
## 
## - best performance: 0.05185402 
## 
## - Detailed performance results:
##     cost gamma      error  dispersion
## 1  1e-01   0.5 0.08956250 0.020328091
## 2  1e+00   0.5 0.05185402 0.019300701
## 3  1e+01   0.5 0.05444809 0.020004784
## 4  1e+02   0.5 0.05440952 0.019873212
## 5  1e+03   0.5 0.05440952 0.019873212
## 6  1e-01   1.0 0.29833830 0.045594412
## 7  1e+00   1.0 0.10569149 0.017702338
## 8  1e+01   1.0 0.10820856 0.019155256
## 9  1e+02   1.0 0.10820856 0.019155256
## 10 1e+03   1.0 0.10820856 0.019155256
## 11 1e-01   2.0 0.38644055 0.063495741
## 12 1e+00   2.0 0.21223559 0.009025986
## 13 1e+01   2.0 0.21278064 0.009302493
## 14 1e+02   2.0 0.21278064 0.009302493
## 15 1e+03   2.0 0.21278064 0.009302493
## 16 1e-01   3.0 0.39238426 0.064471850
## 17 1e+00   3.0 0.23272357 0.007407253
## 18 1e+01   3.0 0.23283600 0.007396949
## 19 1e+02   3.0 0.23283600 0.007396949
## 20 1e+03   3.0 0.23283600 0.007396949
## 21 1e-01   4.0 0.39334840 0.064761426
## 22 1e+00   4.0 0.23681328 0.006531466
## 23 1e+01   4.0 0.23683382 0.006515919
## 24 1e+02   4.0 0.23683382 0.006515919
## 25 1e+03   4.0 0.23683382 0.006515919
```

```r
tune.out2$best.model
```

```
## 
## Call:
## best.tune(method = svm, train.x = y ~ ., data = Auto[train, ], 
##     ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 
##         1, 2, 3, 4)), kernel = "radial")
## 
## 
## Parameters:
##    SVM-Type:  eps-regression 
##  SVM-Kernel:  radial 
##        cost:  1 
##       gamma:  0.5 
##     epsilon:  0.1 
## 
## 
## Number of Support Vectors:  236
```

```r
# poly 
tune.out3=tune(svm, y~., data=Auto[train,], kernel="poly",ranges=list(cost=c(0.1,1,10,100,1000),degree=c(2,3,4)))
summary(tune.out3) # 0.15
```

```
## 
## Parameter tuning of 'svm':
## 
## - sampling method: 10-fold cross validation 
## 
## - best parameters:
##  cost degree
##  1000      3
## 
## - best performance: 0.1506076 
## 
## - Detailed performance results:
##     cost degree     error dispersion
## 1  1e-01      2 0.4667055 0.06829165
## 2  1e+00      2 0.4566331 0.07247091
## 3  1e+01      2 0.3770810 0.09263398
## 4  1e+02      2 0.2399638 0.04938827
## 5  1e+03      2 0.1583750 0.02921011
## 6  1e-01      3 0.4672495 0.06783804
## 7  1e+00      3 0.4617283 0.06821940
## 8  1e+01      3 0.4099117 0.07058503
## 9  1e+02      3 0.1993490 0.03684104
## 10 1e+03      3 0.1506076 0.03060119
## 11 1e-01      4 0.4678574 0.06779994
## 12 1e+00      4 0.4677729 0.06785149
## 13 1e+01      4 0.4669717 0.06839371
## 14 1e+02      4 0.4590837 0.07357824
## 15 1e+03      4 0.3972625 0.10107148
```

```r
tune.out3$best.model
```

```
## 
## Call:
## best.tune(method = svm, train.x = y ~ ., data = Auto[train, ], 
##     ranges = list(cost = c(0.1, 1, 10, 100, 1000), degree = c(2, 
##         3, 4)), kernel = "poly")
## 
## 
## Parameters:
##    SVM-Type:  eps-regression 
##  SVM-Kernel:  polynomial 
##        cost:  1000 
##      degree:  3 
##       gamma:  0.003205128 
##      coef.0:  0 
##     epsilon:  0.1 
## 
## 
## Number of Support Vectors:  290
```

```r
# radial performs the best 
```

(d) Make some plots to back up your assertions in (b) and (c).
Hint: In the lab, we used the plot() function for svm objects only in cases with p = 2. When p > 2, you can use the plot() function to create plots displaying pairs of variables at a time. Essentially, instead of typing


```r
plot(tune.out1$best.model, dat)
plot(tune.out2$best.model, dat)
plot(tune.out3$best.model, dat)
```

where svmfit contains your fitted model and dat is a data frame containing your data, you can type

```r
# plot(svmfit , dat , x1~x4)
```

in order to plot just the first and fourth variables. However, you must replace x1 and x4 with the correct variable names. To find out more, type ?plot.svm.

### 8
This problem involves the OJ data set which is part of the ISLR package.

(a) Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations.

(b) Fit a support vector classifier to the training data using
cost=0.01, with Purchase as the response and the other variables as predictors. Use the summary() function to produce summary statistics, and describe the results obtained.

(c) What are the training and test error rates?

(d) Use the tune() function to select an optimal cost. Consider val-
ues in the range 0.01 to 10.

(e) Compute the training and test error rates using this new value
for cost.

(f) Repeat parts (b) through (e) using a support vector machine
with a radial kernel. Use the default value for gamma.

(g) Repeat parts (b) through (e) using a support vector machine
with a polynomial kernel. Set degree=2.

(h) Overall, which approach seems to give the best results on this data?
