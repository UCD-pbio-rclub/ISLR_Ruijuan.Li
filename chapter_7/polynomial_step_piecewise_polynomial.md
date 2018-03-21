---
title: "Chapter_7_polynoimial_step_piecewise_polynomial"
author: "Ruijuan Li"
date: "3/21/2018"
output: 
  html_document: 
    keep_md: yes
---

### 6 

6. In this exercise, you will further analyze the Wage data set considered throughout this chapter.

(a) Perform polynomial regression to predict wage using age. Use cross-validation to select the optimal degree d for the polynomial. What degree was chosen, and how does this compare to the results of hypothesis testing using ANOVA? Make a plot of the resulting polynomial fit to the data.


```r
# library(tidyverse)
library(ISLR)
```

```
## Warning: package 'ISLR' was built under R version 3.2.5
```

```r
# fit model with different degrees, and use anova 
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage) 
fit.3=lm(wage~poly(age,3),data=Wage) 
fit.4=lm(wage~poly(age,4),data=Wage) 
fit.5=lm(wage~poly(age,5),data=Wage) 
anova(fit.1,fit.2,fit.3,fit.4,fit.5) # degree three I prefer 
```

```
## Analysis of Variance Table
## 
## Model 1: wage ~ age
## Model 2: wage ~ poly(age, 2)
## Model 3: wage ~ poly(age, 3)
## Model 4: wage ~ poly(age, 4)
## Model 5: wage ~ poly(age, 5)
##   Res.Df     RSS Df Sum of Sq        F    Pr(>F)    
## 1   2998 5022216                                    
## 2   2997 4793430  1    228786 143.5931 < 2.2e-16 ***
## 3   2996 4777674  1     15756   9.8888  0.001679 ** 
## 4   2995 4771604  1      6070   3.8098  0.051046 .  
## 5   2994 4770322  1      1283   0.8050  0.369682    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# use cross validation 
library(boot)
```

```
## Warning: package 'boot' was built under R version 3.2.5
```

```r
set.seed(1)
cv.error.10=rep(0,5) 
for (i in 1:5){
glm.fit=glm(wage~poly(age ,i),data=Wage) 
cv.error.10[i]=cv.glm(Wage,glm.fit,K=10)$delta[1] # 10 fold CV 
} 
cv.error.10 # degree four is the best 
```

```
## [1] 1675.837 1601.012 1598.801 1594.217 1594.625
```

```r
# make a plot for polynomial, I prefer result from cv 
# get predict 
agelims=range(Wage$age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit.3,newdata=list(age=age.grid),se=TRUE) # predict
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit ) # predict +/- 2se

# plot 
plot(Wage$age,Wage$wage,xlim=agelims ,cex=.5,col="darkgrey") # plot the value
title("Degree -4 Polynomial ",outer=T) # title 
lines(age.grid,preds$fit,lwd=2,col="blue") # add fit line 
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3) # add CI line 
```

![](polynomial_step_piecewise_polynomial_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

(b) Fit a step function to predict wage using age, and perform cross- validation to choose the optimal number of cuts. Make a plot of the fit obtained.


```r
# cv, for each cut from 2 to 6, run 10 times validation set test, each validation set start with a different seed 
# make a function 
cv.k10.step <- function(Wage, cut_number){
for (i in 1:10){ # k level 
set.seed(i)
train_ID <- sample(rownames(Wage), size = round(nrow(Wage) * 0.5), replace = F) 
train <- Wage[rownames(Wage) %in% train_ID,]
test <- Wage[!(rownames(Wage) %in% train_ID),] 

table(cut(train$age,cut_number)) # cut pick cutpoint automatically or specify cut point using breaks 
fit=lm(wage~cut(train$age,cut_number),data=train) 
agelims=range(train$age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE) # predict
cv.error[i] <- mean((test$wage-preds$fit)^2)
}  
mean(cv.error)} 

# run cv on step function, cut from 2 to 6
cv.error=rep(0,5)
for (i in 2:6){
cv.error[i]=cv.k10.step(Wage, i)}

cv.error ### doesn't look right.... 
```

```
## [1]    0.000 1724.302 1774.685 1824.554 1830.562 1841.501
```

```r
# fit step function directly using cut number of 4
fit=lm(wage~cut(age ,4),data=Wage) 
coef(summary(fit))
```

```
##                         Estimate Std. Error   t value     Pr(>|t|)
## (Intercept)            94.158392   1.476069 63.789970 0.000000e+00
## cut(age, 4)(33.5,49]   24.053491   1.829431 13.148074 1.982315e-38
## cut(age, 4)(49,64.5]   23.664559   2.067958 11.443444 1.040750e-29
## cut(age, 4)(64.5,80.1]  7.640592   4.987424  1.531972 1.256350e-01
```

```r
# predict 
agelims=range(Wage$age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE) # predict
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit ) # predict +/- 2se

# plot the fit result 
plot(Wage$age,Wage$wage,xlim=agelims ,cex=.5,col="darkgrey") # plot the value
title("Step fit ",outer=T) # title 
lines(age.grid,preds$fit,lwd=2,col="blue") # add fit line 
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3) # add CI line  
```

![](polynomial_step_piecewise_polynomial_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### 7 

The Wage data set contains a number of other features not explored in this chapter, such as marital status (maritl), job class (jobclass), and others. Explore the relationships between some of these other predictors and wage, and use non-linear fitting techniques in order to fit flexible models to the data. Create plots of the results obtained, and write a summary of your findings.


```r
library(ISLR) 
colnames(Wage) 
```

```
##  [1] "year"       "age"        "maritl"     "race"       "education" 
##  [6] "region"     "jobclass"   "health"     "health_ins" "logwage"   
## [11] "wage"
```

```r
pairs(Wage)
```

![](polynomial_step_piecewise_polynomial_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
Wage$maritl <- as.numeric(Wage$maritl)
# use maritial status to predict wage, categorical predictor, I decided to use step function  
table(cut(Wage$maritl,4)) # cut pick cutpoint automatically or specify cut point using breaks 
```

```
## 
## (0.996,2]     (2,3]     (3,4]     (4,5] 
##      2722        19       204        55
```

```r
fit=lm(wage~cut(maritl ,4),data=Wage) 
coef(summary(fit))
```

```
##                      Estimate Std. Error    t value    Pr(>|t|)
## (Intercept)         112.64079  0.7982099 141.116753 0.000000000
## cut(maritl, 4)(2,3] -13.10214  9.5872672  -1.366618 0.171847494
## cut(maritl, 4)(3,4]  -9.48153  3.0230073  -3.136456 0.001726615
## cut(maritl, 4)(4,5] -11.42500  5.6718366  -2.014339 0.044063117
```

```r
# predict 
agelims=range(Wage$maritl)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(maritl=age.grid),se=TRUE) # predict
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit ) # predict +/- 2se

# plot the fit result 
plot(Wage$maritl,Wage$wage,xlim=agelims ,cex=.5,col="darkgrey") # plot the value
title("Step fit marital ",outer=T) # title 
lines(age.grid,preds$fit,lwd=2,col="blue") # add fit line 
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3) # add CI line  
```

![](polynomial_step_piecewise_polynomial_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
# to make money, either don't marry or be married for ever 
# 1. Never Married 2. Married 3. Widowed 4. Divorced 5. Separated maybe this level, need to check 
```

### 8 

Fit some of the non-linear models investigated in this chapter to the Auto data set. Is there evidence for non-linear relationships in this data set? Create some informative plots to justify your answer.


```r
colnames(Auto) 
```

```
## [1] "mpg"          "cylinders"    "displacement" "horsepower"  
## [5] "weight"       "acceleration" "year"         "origin"      
## [9] "name"
```

```r
# I would like to invesitigate the relationship between origin and mpg, hypothesis, Japanese car has a higher mpg, origin matters 

# polynomial 
fit=lm(mpg~poly(origin,2),data=Auto) 
summary(fit) # degree of two 
```

```
## 
## Call:
## lm(formula = mpg ~ poly(origin, 2), data = Auto)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.451  -5.034  -1.034   3.649  18.966 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       23.4459     0.3231  72.575  < 2e-16 ***
## poly(origin, 2)1  87.2309     6.3962  13.638  < 2e-16 ***
## poly(origin, 2)2 -17.1771     6.3962  -2.686  0.00755 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.396 on 389 degrees of freedom
## Multiple R-squared:  0.3318,	Adjusted R-squared:  0.3284 
## F-statistic:  96.6 on 2 and 389 DF,  p-value: < 2.2e-16
```

```r
# step 
table(cut(Auto$origin, breaks = 4)) # cut pick cutpoint automatically or specify cut point using breaks 
```

```
## 
## (0.998,1.5]     (1.5,2]     (2,2.5]     (2.5,3] 
##         245          68           0          79
```

```r
fit=lm(mpg~cut(origin ,4),data=Auto)  
coef(summary(fit))
```

```
##                        Estimate Std. Error   t value      Pr(>|t|)
## (Intercept)           20.033469  0.4086405 49.024678 1.383741e-168
## cut(origin, 4)(1.5,2]  7.569472  0.8767164  8.633889  1.543152e-16
## cut(origin, 4)(2.5,3] 10.417164  0.8275617 12.587779  1.023502e-30
```

```r
# predict 
agelims=range(Auto$origin)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(origin=age.grid),se=TRUE) # predict
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit ) # predict +/- 2se

# plot the fit result 
plot(Auto$origin,Auto$mpg,xlim=agelims ,cex=.5,col="darkgrey") # plot the value
title("Step fit mpg vs. origin ",outer=T) # title 
lines(age.grid,preds$fit,lwd=2,col="blue") # add fit line 
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3) # add CI line, origin matters 
```

![](polynomial_step_piecewise_polynomial_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# splines 
library(splines)
fit=lm(mpg~bs(origin,knots=c(1,2,3)),data=Auto) # specify knots at 25, 40, and 60 
pred.splines=predict(fit,newdata=list(origin=age.grid),se=T)
```

```
## Warning in predict.lm(fit, newdata = list(origin = age.grid), se = T):
## prediction from a rank-deficient fit may be misleading
```

```r
pred.splines 
```

```
## $fit
##        1        2        3 
## 20.03347 27.60294 30.45063 
## 
## $se.fit
##         1         2         3 
## 0.4086405 0.7756575 0.7196327 
## 
## $df
## [1] 389
## 
## $residual.scale
## [1] 6.396236
```

```r
plot(Auto$origin,Auto$mpg,col="gray")
lines(age.grid,pred.splines$fit,lwd=2)
lines(age.grid,pred.splines$fit+2*pred.splines$se ,lty="dashed")
lines(age.grid,pred.splines$fit-2*pred.splines$se ,lty="dashed")

# natural splines
fit2=lm(mpg~ns(origin,df=4),data=Auto)
pred2=predict(fit2,newdata=list(origin=age.grid),se=T) 
```

```
## Warning in predict.lm(fit2, newdata = list(origin = age.grid), se = T):
## prediction from a rank-deficient fit may be misleading
```

```r
lines(age.grid, pred2$fit,col="red",lwd=2)  
```

![](polynomial_step_piecewise_polynomial_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

### 9 

This question uses the variables dis (the weighted mean of distances to five Boston employment centers) and nox (nitrogen oxides concen- tration in parts per 10 million) from the Boston data. We will treat dis as the predictor and nox as the response.

(a) Use the poly() function to fit a cubic polynomial regression to predict nox using dis. Report the regression output, and plot the resulting data and polynomial fits.


```r
library(MASS)
```

```
## Warning: package 'MASS' was built under R version 3.2.5
```

```r
colnames(Boston)
```

```
##  [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
##  [8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"
```

```r
fit=lm(nox~poly(dis,3),data=Boston) 
summary(fit) 
```

```
## 
## Call:
## lm(formula = nox ~ poly(dis, 3), data = Boston)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.121130 -0.040619 -0.009738  0.023385  0.194904 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    0.554695   0.002759 201.021  < 2e-16 ***
## poly(dis, 3)1 -2.003096   0.062071 -32.271  < 2e-16 ***
## poly(dis, 3)2  0.856330   0.062071  13.796  < 2e-16 ***
## poly(dis, 3)3 -0.318049   0.062071  -5.124 4.27e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.06207 on 502 degrees of freedom
## Multiple R-squared:  0.7148,	Adjusted R-squared:  0.7131 
## F-statistic: 419.3 on 3 and 502 DF,  p-value: < 2.2e-16
```

```r
lims=range(Boston$dis)
grid=seq(from=lims[1],to=lims[2])
preds=predict(fit,newdata=list(dis=grid),se=TRUE) # predict
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit ) # predict +/- 2se

plot(Boston$dis,Boston$nox,xlim=lims ,cex=.5,col="darkgrey") # plot the value
title("Degree -3 Polynomial ",outer=T) # title 
lines(grid,preds$fit,lwd=2,col="blue") # add fit line 
matlines(grid,se.bands,lwd=1,col="blue",lty=3) # add CI line  
```

![](polynomial_step_piecewise_polynomial_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

(b) Plot the polynomial fits for a range of different polynomial degrees (say, from 1 to 10), and report the associated residual sum of squares.


```r
# what is rss? residual: the difference between the ith observed response value and the ith response value that is predicted by our linear model. RSS is sum of squares of residuals 
set.seed(1)
cv.error.10=rep(0,10) 
for (i in 1:10){
glm.fit=glm(nox~poly(dis ,i),data=Boston) 
cv.error.10[i]= sum((Boston$nox-predict(glm.fit, Boston))^2) 
} 
cv.error.10  
```

```
##  [1] 2.768563 2.035262 1.934107 1.932981 1.915290 1.878257 1.849484
##  [8] 1.835630 1.833331 1.832171
```

```r
plot(cv.error.10) # degree of 3 
```

![](polynomial_step_piecewise_polynomial_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

(c) Perform cross-validation or another approach to select the opti- mal degree for the polynomial, and explain your results.


```r
cv.error=rep(0,5) 
for (i in 1:5){
glm.fit=glm(nox~poly(dis ,i),data=Boston) # different polynomial levels 
cv.error[i]=cv.glm(Boston,glm.fit, K=10)$delta[1]}  
cv.error  
```

```
## [1] 0.005536329 0.004077147 0.003899587 0.003862127 0.004298590
```

```r
plot(cv.error) # degree of 3 
```

![](polynomial_step_piecewise_polynomial_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

I will stop here. 

(d) Use the bs() function to fit a regression spline to predict nox using dis. Report the output for the fit using four degrees of freedom. How did you choose the knots? Plot the resulting fit.



(e) Now fit a regression spline for a range of degrees of freedom, and plot the resulting fits and report the resulting RSS. Describe the results obtained.

(f) Perform cross-validation or another approach in order to select the best degrees of freedom for a regression spline on this data. Describe your results.
