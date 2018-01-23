---
title: "Assignment_01_23_2018"
author: "Ruijuan Li"
date: "1/23/2018"
output: 
  html_document: 
    keep_md: yes
---
* I don't know what I am doing here... 

10. This question should be answered using the Weekly data set, which is part of the ISLR package. This data is similar in nature to the Smarket data from this chapter’s lab, except that it contains 1,089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010.

(e) Repeat (d) using LDA. (d, fit the logistic regression model using a training data period from 1990 to 2008, with Lag2 as the only predictor. Compute the confusion matrix and the overall fraction of correct predictions for the held out data (that is, the data from 2009 and 2010).)


```r
library(MASS)
```

```
## Warning: package 'MASS' was built under R version 3.2.5
```

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
## ✖ dplyr::select() masks MASS::select()
```

```r
library(ISLR)
```

```
## Warning: package 'ISLR' was built under R version 3.2.5
```

```r
Weekly %>% summary() 
```

```
##       Year           Lag1               Lag2               Lag3         
##  Min.   :1990   Min.   :-18.1950   Min.   :-18.1950   Min.   :-18.1950  
##  1st Qu.:1995   1st Qu.: -1.1540   1st Qu.: -1.1540   1st Qu.: -1.1580  
##  Median :2000   Median :  0.2410   Median :  0.2410   Median :  0.2410  
##  Mean   :2000   Mean   :  0.1506   Mean   :  0.1511   Mean   :  0.1472  
##  3rd Qu.:2005   3rd Qu.:  1.4050   3rd Qu.:  1.4090   3rd Qu.:  1.4090  
##  Max.   :2010   Max.   : 12.0260   Max.   : 12.0260   Max.   : 12.0260  
##       Lag4               Lag5              Volume       
##  Min.   :-18.1950   Min.   :-18.1950   Min.   :0.08747  
##  1st Qu.: -1.1580   1st Qu.: -1.1660   1st Qu.:0.33202  
##  Median :  0.2380   Median :  0.2340   Median :1.00268  
##  Mean   :  0.1458   Mean   :  0.1399   Mean   :1.57462  
##  3rd Qu.:  1.4090   3rd Qu.:  1.4050   3rd Qu.:2.05373  
##  Max.   : 12.0260   Max.   : 12.0260   Max.   :9.32821  
##      Today          Direction 
##  Min.   :-18.1950   Down:484  
##  1st Qu.: -1.1540   Up  :605  
##  Median :  0.2410             
##  Mean   :  0.1499             
##  3rd Qu.:  1.4050             
##  Max.   : 12.0260
```

```r
train = (Weekly$Year >= 1990 & Weekly$Year <=  2008)
lda.fit.1=lda(Direction ~ Lag2,data=Weekly, subset=train)  
lda.fit.1
```

```
## Call:
## lda(Direction ~ Lag2, data = Weekly, subset = train)
## 
## Prior probabilities of groups:
##      Down        Up 
## 0.4477157 0.5522843 
## 
## Group means:
##             Lag2
## Down -0.03568254
## Up    0.26036581
## 
## Coefficients of linear discriminants:
##            LD1
## Lag2 0.4414162
```

```r
Weekly.2005= Weekly[!train,]
lda.pred.1=predict(lda.fit.1, Weekly.2005) 

Direction.2005=Weekly$Direction[!train]
lda.class.1=lda.pred.1$class
table(lda.class.1 ,Direction.2005)
```

```
##            Direction.2005
## lda.class.1 Down Up
##        Down    9  5
##        Up     34 56
```

```r
mean(lda.class.1==Direction.2005)
```

```
## [1] 0.625
```

```r
sum(lda.pred.1$posterior[,1]>=.5) 
```

```
## [1] 14
```

```r
sum(lda.pred.1$posterior[,1]<.5)
```

```
## [1] 90
```

```r
lda.pred.1$posterior[1:20,1] 
```

```
##       986       987       988       989       990       991       992 
## 0.4736555 0.3558617 0.5132860 0.5142948 0.4799727 0.4597586 0.3771117 
##       993       994       995       996       997       998       999 
## 0.5184724 0.5480397 0.5146118 0.5504246 0.3055404 0.4268160 0.3637275 
##      1000      1001      1002      1003      1004      1005 
## 0.4034316 0.4256310 0.4277053 0.4548626 0.4308002 0.3674066
```

```r
lda.class.1[1:20]
```

```
##  [1] Up   Up   Down Down Up   Up   Up   Down Down Down Down Up   Up   Up  
## [15] Up   Up   Up   Up   Up   Up  
## Levels: Down Up
```

```r
sum(lda.pred.1$posterior[,1]>.9)   
```

```
## [1] 0
```

11. In this problem, you will develop a model to predict whether a given car gets high or low gas mileage based on the Auto data set.
 
(d) Perform LDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained? 


```r
# using weight, displacement, horsepower, ang cylinders as the predictors 

Auto %>% summary()
```

```
##       mpg          cylinders      displacement     horsepower   
##  Min.   : 9.00   Min.   :3.000   Min.   : 68.0   Min.   : 46.0  
##  1st Qu.:17.00   1st Qu.:4.000   1st Qu.:105.0   1st Qu.: 75.0  
##  Median :22.75   Median :4.000   Median :151.0   Median : 93.5  
##  Mean   :23.45   Mean   :5.472   Mean   :194.4   Mean   :104.5  
##  3rd Qu.:29.00   3rd Qu.:8.000   3rd Qu.:275.8   3rd Qu.:126.0  
##  Max.   :46.60   Max.   :8.000   Max.   :455.0   Max.   :230.0  
##                                                                 
##      weight      acceleration        year           origin     
##  Min.   :1613   Min.   : 8.00   Min.   :70.00   Min.   :1.000  
##  1st Qu.:2225   1st Qu.:13.78   1st Qu.:73.00   1st Qu.:1.000  
##  Median :2804   Median :15.50   Median :76.00   Median :1.000  
##  Mean   :2978   Mean   :15.54   Mean   :75.98   Mean   :1.577  
##  3rd Qu.:3615   3rd Qu.:17.02   3rd Qu.:79.00   3rd Qu.:2.000  
##  Max.   :5140   Max.   :24.80   Max.   :82.00   Max.   :3.000  
##                                                                
##                  name    
##  amc matador       :  5  
##  ford pinto        :  5  
##  toyota corolla    :  5  
##  amc gremlin       :  4  
##  amc hornet        :  4  
##  chevrolet chevette:  4  
##  (Other)           :365
```

```r
# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its median, and a 0 if mpg contains a value below its median. You can compute the median using the median() function. Note you may find it helpful to use the data.frame() function to create a single data set containing both mpg01 and the other Auto variables.

attach(Auto)
```

```
## The following object is masked from package:ggplot2:
## 
##     mpg
```

```r
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto2 <- data.frame(Auto, mpg01)

lda.fit.2=lda(mpg01 ~ weight + displacement + horsepower + cylinders,data=Auto2)  
lda.fit.2 
```

```
## Call:
## lda(mpg01 ~ weight + displacement + horsepower + cylinders, data = Auto2)
## 
## Prior probabilities of groups:
##   0   1 
## 0.5 0.5 
## 
## Group means:
##     weight displacement horsepower cylinders
## 0 3620.403     273.1582  130.11224  6.765306
## 1 2334.765     115.6658   78.82653  4.178571
## 
## Coefficients of linear discriminants:
##                        LD1
## weight       -0.0009846242
## displacement -0.0014339977
## horsepower    0.0044241441
## cylinders    -0.4708126926
```

