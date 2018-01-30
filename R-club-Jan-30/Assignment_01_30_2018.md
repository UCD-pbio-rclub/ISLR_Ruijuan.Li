---
title: "Assignment_01_30_2018"
author: "Ruijuan Li"
date: "1/29/2018"
output: 
  html_document: 
    keep_md: yes
---

### 5. 

We now examine the differences between LDA and QDA.

(a) If the Bayes decision boundary is linear, do we expect LDA or QDA to perform better on the training set? On the test set?

LDA better on the test set and training set 

(b) If the Bayes decision boundary is non-linear, do we expect LDA or QDA to perform better on the training set? On the test set?

QDA better on the test set and training set 

(c) In general, as the sample size n increases, do we expect the test prediction accuracy of QDA relative to LDA to improve, decline, or be unchanged? Why? 

improve, because "LDA tends to be a better bet than QDA if there are relatively few training observations and so reducing variance is crucial. In contrast, QDA is recommended if the training set is very large, so that the variance of the classifier is not a major concern, or if the assumption of a common covariance matrix for the K classes is clearly untenable."

(d) True or False: Even if the Bayes decision boundary for a given problem is linear, we will probably achieve a superior test er- ror rate using QDA rather than LDA because QDA is flexible enough to model a linear decision boundary. Justify your answer.

False. Scenario 3 where the underline distribution violated normal distribution resulted a high error rate for QDA.  

### 8.

Suppose that we take a data set, divide it into equally-sized training and test sets, and then try out two different classification procedures. First we use logistic regression and get an error rate of 20 % on the training data and 30 % on the test data. Next we use 1-nearest neigh- bors (i.e. K = 1) and get an average error rate (averaged over both test and training data sets) of 18%. Based on these results, which method should we prefer to use for classification of new observations? Why?

Depends on the data, it can be that KNN has very small error for training set but very large error for test set (>30%) when the data are sampled from a more complicated non-linear function (scenario 6). So although KNN performes well on the training set, it has very large error for test set. So to be safe, I would select logistic regression unless the bayes decision boundary is very unlinear (Scenoria 5). 

### 9. 

This problem has to do with odds.

(a) On average, what fraction of people with an odds of 0.37 of defaulting on their credit card payment will in fact default?

0.37 odds ratio means for a person the probability of default VS non-default is 0.37. 

```r
0.37/(1+0.37) # 27% 
```

```
## [1] 0.270073
```

(b) Suppose that an individual has a 16% chance of defaulting on her credit card payment. What are the odds that she will de- fault?


```r
0.16/(1-0.16) # 0.19 
```

```
## [1] 0.1904762
```

### Applied  

### 10 

(f) Repeat (d) using QDA.

(g) Repeat (d) using KNN with K = 1.

(h) Which of these methods appears to provide the best results on this data?

(i) Experiment with different combinations of predictors, including possible transformations and interactions, for each of the methods. Report the variables, method, and associated confu- sion matrix that appears to provide the best results on the held out data. Note that you should also experiment with values for K in the KNN classifier.


```r
# a) 

library(GGally)
```

```
## Warning: package 'GGally' was built under R version 3.2.5
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
summary(Weekly)
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
dim(Weekly) # 1089 
```

```
## [1] 1089    9
```

```r
attach(Weekly) 

# ggpairs(Weekly, aes(color = Direction)) # seems like not obvious correlation with direction for any of the possible predictors here 

# b) 
glm.fits.1=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly ,family=binomial)
summary(glm.fits.1) # Lag2  
```

```
## 
## Call:
## glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
##     Volume, family = binomial, data = Weekly)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.6949  -1.2565   0.9913   1.0849   1.4579  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept)  0.26686    0.08593   3.106   0.0019 **
## Lag1        -0.04127    0.02641  -1.563   0.1181   
## Lag2         0.05844    0.02686   2.175   0.0296 * 
## Lag3        -0.01606    0.02666  -0.602   0.5469   
## Lag4        -0.02779    0.02646  -1.050   0.2937   
## Lag5        -0.01447    0.02638  -0.549   0.5833   
## Volume      -0.02274    0.03690  -0.616   0.5377   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1496.2  on 1088  degrees of freedom
## Residual deviance: 1486.4  on 1082  degrees of freedom
## AIC: 1500.4
## 
## Number of Fisher Scoring iterations: 4
```

```r
# c) 
glm.probs=predict(glm.fits.1,type="response")

glm.pred=rep("Down",1089)
glm.pred[glm.probs >.5]="Up"

table(glm.pred,Direction) 
```

```
##         Direction
## glm.pred Down  Up
##     Down   54  48
##     Up    430 557
```

```r
48 + 430 / 1089 # 48% error rate, which the model does not have the right prediction 
```

```
## [1] 48.39486
```

```r
# d) 
train =( Year >= 1990 & Year <= 2008)
Weekly.test= Weekly[!train ,]
dim(Weekly.test) # 104 
```

```
## [1] 104   9
```

```r
train %>% sum()
```

```
## [1] 985
```

```r
Direction.test=Direction[!train]

glm.fits.2=glm(Direction ~ Lag2, data=Weekly,family=binomial,subset=train)

glm.probs=predict(glm.fits.2,Weekly.test,type="response")

glm.pred=rep("Down",104)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.test)
```

```
##         Direction.test
## glm.pred Down Up
##     Down    9  5
##     Up     34 56
```

```r
(5+34)/104  # 37.5 test error rate  
```

```
## [1] 0.375
```

```r
# e) 
lda.fit.1=lda(Direction ~ Lag2,data=Weekly, subset=train)   

test= Weekly[!train,] 
lda.pred.1=predict(lda.fit.1, test) 

Direction.test=Weekly$Direction[!train]
lda.class.1=lda.pred.1$class
table(lda.class.1 ,Direction.test)
```

```
##            Direction.test
## lda.class.1 Down Up
##        Down    9  5
##        Up     34 56
```

```r
(5+34)/104 # same error rate as in d, check error rate on test data 
```

```
## [1] 0.375
```

```r
# f) 
qda.fit.1=qda(Direction ~ Lag2,data=Weekly ,subset=train) # fit data on training set 
qda.fit.1 # check fit summary, don't understand the summary result 
```

```
## Call:
## qda(Direction ~ Lag2, data = Weekly, subset = train)
## 
## Prior probabilities of groups:
##      Down        Up 
## 0.4477157 0.5522843 
## 
## Group means:
##             Lag2
## Down -0.03568254
## Up    0.26036581
```

```r
qda.pred.1 = predict(qda.fit.1, test)
qda.class.1=qda.pred.1$class 
table(qda.class.1 ,Direction.test)
```

```
##            Direction.test
## qda.class.1 Down Up
##        Down    0  0
##        Up     43 61
```

```r
43/104 # 41% higher than the LDA & logistic regression 
```

```
## [1] 0.4134615
```

```r
# g) 
train.1=(Lag2[train]) %>% as.matrix() # the P Lag2, traning set 
test.1=Lag2[!train] %>% as.matrix() # test set
train.Direction =Direction [train] # direction for training set 
train.Direction 
```

```
##   [1] Down Down Up   Up   Up   Down Up   Up   Up   Down Down Up   Up   Up  
##  [15] Down Up   Down Up   Down Up   Up   Up   Down Down Down Down Down Down
##  [29] Up   Up   Down Down Down Up   Down Up   Down Up   Up   Up   Down Up  
##  [43] Up   Down Up   Down Down Down Up   Up   Up   Up   Up   Down Up   Up  
##  [57] Down Down Up   Up   Up   Up   Down Up   Down Down Up   Up   Down Up  
##  [71] Down Down Up   Up   Up   Down Up   Down Down Up   Up   Down Down Up  
##  [85] Down Down Up   Up   Down Up   Up   Down Down Down Up   Up   Up   Up  
##  [99] Up   Down Up   Down Down Up   Up   Down Up   Down Up   Up   Down Down
## [113] Up   Up   Down Up   Up   Down Up   Up   Down Down Down Down Up   Up  
## [127] Up   Down Up   Down Up   Down Down Up   Up   Up   Down Down Down Up  
## [141] Up   Up   Down Up   Up   Up   Up   Up   Up   Down Down Down Up   Down
## [155] Up   Up   Down Down Up   Up   Up   Up   Down Down Up   Up   Down Up  
## [169] Up   Down Up   Up   Down Down Down Up   Down Up   Down Up   Up   Up  
## [183] Up   Up   Up   Up   Up   Down Down Up   Down Up   Down Up   Down Up  
## [197] Down Up   Up   Down Up   Up   Down Up   Up   Down Up   Down Up   Down
## [211] Down Down Up   Up   Down Down Up   Down Up   Up   Down Down Up   Up  
## [225] Up   Down Down Down Up   Up   Up   Down Up   Down Up   Up   Up   Down
## [239] Down Up   Down Up   Down Up   Down Up   Down Up   Down Down Up   Down
## [253] Up   Up   Down Up   Up   Down Up   Up   Up   Up   Up   Down Up   Up  
## [267] Up   Down Up   Up   Down Up   Up   Up   Down Up   Up   Down Up   Up  
## [281] Down Up   Up   Down Up   Down Down Up   Up   Up   Up   Up   Down Up  
## [295] Down Up   Up   Down Up   Up   Up   Down Up   Up   Down Down Up   Up  
## [309] Down Up   Up   Up   Up   Down Up   Down Down Up   Up   Down Up   Down
## [323] Up   Up   Down Up   Up   Up   Down Up   Down Up   Up   Down Down Down
## [337] Down Up   Down Up   Up   Down Up   Up   Up   Down Up   Down Up   Down
## [351] Up   Up   Up   Up   Up   Down Down Up   Up   Down Up   Up   Down Up  
## [365] Up   Up   Down Down Up   Down Down Down Down Down Up   Down Up   Up  
## [379] Up   Up   Up   Up   Up   Up   Down Up   Down Down Up   Up   Down Down
## [393] Up   Down Up   Down Up   Down Up   Up   Down Down Down Up   Up   Up  
## [407] Down Up   Down Down Down Up   Down Up   Down Up   Up   Up   Up   Up  
## [421] Up   Up   Up   Down Up   Down Up   Down Up   Down Up   Up   Down Up  
## [435] Down Up   Up   Up   Up   Up   Down Down Down Down Up   Down Down Up  
## [449] Up   Up   Down Down Up   Up   Up   Up   Down Up   Up   Down Down Up  
## [463] Up   Up   Up   Down Down Up   Down Down Up   Down Up   Up   Up   Down
## [477] Up   Up   Down Up   Down Up   Down Down Down Up   Down Up   Down Up  
## [491] Up   Up   Down Down Down Up   Up   Up   Up   Down Down Down Up   Up  
## [505] Down Up   Up   Up   Up   Up   Down Up   Down Up   Up   Up   Down Up  
## [519] Down Down Up   Down Down Down Up   Down Up   Up   Down Up   Down Up  
## [533] Up   Down Down Down Down Up   Down Up   Down Up   Up   Up   Down Down
## [547] Up   Up   Up   Up   Up   Down Down Down Down Down Down Up   Down Up  
## [561] Down Up   Down Down Up   Down Down Up   Down Up   Up   Up   Down Down
## [575] Down Down Down Down Down Down Up   Down Up   Up   Up   Up   Down Up  
## [589] Down Down Up   Down Up   Down Down Up   Down Down Up   Down Down Up  
## [603] Down Down Down Up   Up   Up   Down Up   Down Up   Up   Up   Down Up  
## [617] Down Up   Up   Up   Down Down Up   Down Down Up   Down Up   Up   Up  
## [631] Down Down Down Down Up   Down Down Down Up   Down Down Down Down Down
## [645] Up   Down Down Down Up   Up   Up   Up   Up   Down Down Down Down Down
## [659] Down Up   Up   Up   Up   Down Up   Up   Up   Down Down Up   Down Up  
## [673] Up   Down Down Down Down Up   Up   Down Down Up   Up   Down Up   Down
## [687] Up   Up   Up   Up   Up   Down Up   Up   Up   Up   Down Up   Up   Down
## [701] Up   Down Down Up   Up   Up   Up   Down Up   Down Up   Up   Up   Down
## [715] Up   Up   Down Down Up   Up   Up   Up   Up   Up   Up   Up   Up   Down
## [729] Up   Up   Down Up   Up   Down Down Down Up   Down Down Up   Down Down
## [743] Down Down Up   Up   Up   Down Down Down Down Down Down Up   Down Up  
## [757] Up   Up   Up   Up   Up   Down Up   Down Down Down Up   Up   Up   Down
## [771] Up   Up   Down Up   Up   Up   Down Down Down Up   Up   Up   Down Up  
## [785] Up   Down Down Down Up   Up   Down Up   Up   Up   Down Up   Up   Down
## [799] Up   Up   Down Up   Up   Up   Up   Up   Down Up   Down Down Up   Up  
## [813] Down Down Up   Down Down Down Up   Up   Up   Up   Up   Down Down Up  
## [827] Up   Down Up   Up   Down Up   Down Up   Up   Up   Down Down Up   Down
## [841] Down Up   Down Up   Down Up   Down Down Up   Up   Down Down Down Up  
## [855] Down Down Up   Up   Up   Down Up   Down Up   Down Up   Down Up   Up  
## [869] Up   Up   Up   Down Up   Up   Down Down Up   Up   Down Up   Down Up  
## [883] Down Down Up   Down Up   Down Down Up   Down Up   Down Up   Up   Up  
## [897] Up   Up   Up   Up   Down Up   Down Up   Down Up   Up   Up   Down Down
## [911] Down Up   Down Up   Down Down Up   Up   Up   Up   Up   Down Up   Down
## [925] Down Up   Down Up   Up   Down Up   Down Down Down Down Up   Up   Down
## [939] Up   Up   Down Down Down Up   Down Up   Down Up   Up   Up   Down Up  
## [953] Down Up   Down Down Down Down Down Down Up   Down Up   Up   Up   Down
## [967] Down Down Up   Up   Down Down Down Up   Down Up   Down Down Down Up  
## [981] Down Up   Up   Down Up  
## Levels: Down Up
```

```r
# set.seed(1) # why set.seed() ?
# knn.pred.1=knn(train.1,test.1,train.Direction ,k=1)
# table(knn.pred.1,Direction.test)

(30+22)/104 # 50% error rate, highest among the 4 
```

```
## [1] 0.5
```

```r
# h) 
# logistic & LDA gave the same best result 

# i) try differnt k here 
# set.seed(1) # why set.seed() ?
# knn.pred.2=knn(train.1,test.1,train.Direction ,k=3)
# table(knn.pred.2,Direction.test)

(20+27)/104 # 45% error rate
```

```
## [1] 0.4519231
```

```r
# set.seed(1) # why set.seed() ?
# knn.pred.3=knn(train.1,test.1,train.Direction ,k=5)
# table(knn.pred.3,Direction.test)

(21+27)/104 # 46% error rate 
```

```
## [1] 0.4615385
```

### 11

(e) Perform QDA on the training data in order to predict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?

(f) Perform logistic regression on the training data in order to pre- dict mpg01 using the variables that seemed most associated with mpg01 in (b). What is the test error of the model obtained?

(g) Perform KNN on the training data, with several values of K, in order to predict mpg01. Use only the variables that seemed most associated with mpg01 in (b). What test errors do you obtain? Which value of K seems to perform the best on this data set?


```r
# a) 
Auto2 <- 
Auto %>% 
  mutate(mpg01 = as.factor(ifelse(mpg > median(mpg), 1, 0))) 
```

```
## Warning: package 'bindrcpp' was built under R version 3.2.5
```

```r
# b) 
# Auto2 %>% 
#   dplyr::select(-name) %>% 
#   ggpairs(aes(color = mpg01)) # mpg, displacement, horsepower, weight 

# c) 
# decided to use 80% as training set 
Auto %>% nrow() * 0.8
```

```
## [1] 313.6
```

```r
set.seed(1)
train_ID <- sample(rownames(Auto), size = round(nrow(Auto) * 0.8), replace = F) 
train <- Auto2[rownames(Auto) %in% train_ID,]
test <- Auto2[!(rownames(Auto) %in% train_ID),] 

dim(train)
```

```
## [1] 314  10
```

```r
dim(test) # 78
```

```
## [1] 78 10
```

```r
# d) 
attach(Auto2)
```

```
## The following object is masked from package:ggplot2:
## 
##     mpg
```

```r
lda.fit.2=lda(mpg01 ~ weight + displacement + horsepower + cylinders,data=train)  
lda.fit.2 
```

```
## Call:
## lda(mpg01 ~ weight + displacement + horsepower + cylinders, data = train)
## 
## Prior probabilities of groups:
##         0         1 
## 0.5159236 0.4840764 
## 
## Group means:
##     weight displacement horsepower cylinders
## 0 3602.512     269.5556  128.50617  6.672840
## 1 2350.526     117.4145   78.57895  4.210526
## 
## Coefficients of linear discriminants:
##                        LD1
## weight       -0.0009498732
## displacement -0.0016911294
## horsepower    0.0030863992
## cylinders    -0.4177755586
```

```r
lda.pred.2=predict(lda.fit.2, test) 

mpg01.test <- test$mpg01 
lda.class.2=lda.pred.2$class
table(lda.class.2 ,mpg01.test)
```

```
##            mpg01.test
## lda.class.2  0  1
##           0 30  1
##           1  4 43
```

```r
(1+4)/78 # 6% test error rate 
```

```
## [1] 0.06410256
```

```r
# e) 
qda.fit.2=qda(mpg01 ~ weight + displacement + horsepower + cylinders,data=train)  
qda.fit.2 
```

```
## Call:
## qda(mpg01 ~ weight + displacement + horsepower + cylinders, data = train)
## 
## Prior probabilities of groups:
##         0         1 
## 0.5159236 0.4840764 
## 
## Group means:
##     weight displacement horsepower cylinders
## 0 3602.512     269.5556  128.50617  6.672840
## 1 2350.526     117.4145   78.57895  4.210526
```

```r
qda.pred.2=predict(qda.fit.2, test) 

qda.class.2=qda.pred.2$class
table(qda.class.2 ,mpg01.test)
```

```
##            mpg01.test
## qda.class.2  0  1
##           0 31  4
##           1  3 40
```

```r
(4+3)/78 # 9% test error rate 
```

```
## [1] 0.08974359
```

```r
# f) 
glm.fits.3=glm(mpg01 ~ displacement + horsepower + weight, data=train,family=binomial) 
glm.probs=predict(glm.fits.3,test,type="response")

glm.pred=rep(0,78)
glm.pred[glm.probs >.5]=1
table(glm.pred,test$mpg01)
```

```
##         
## glm.pred  0  1
##        0 30  3
##        1  4 41
```

```r
(3 + 5) / 78 
```

```
## [1] 0.1025641
```

```r
# error rate is very low, 10%  

# g) 
# k = 1 
####  need to standadize here... because multiple predictors 
# standardized.X=scale(Auto2[,c("displacenemnt", "horsepower", "weight")])  

colnames(Auto2)
```

```
##  [1] "mpg"          "cylinders"    "displacement" "horsepower"  
##  [5] "weight"       "acceleration" "year"         "origin"      
##  [9] "name"         "mpg01"
```

```r
train.2 <- train[,c("displacement", "horsepower", "weight")]
test.2 <- test[,c("displacement", "horsepower", "weight")]

mpg01.train =train$mpg01 # direction for training set 

# set.seed(1) # why set.seed() ?
# knn.pred.3=knn(train.2,test.2,mpg01.train ,k=1)
# table(knn.pred.3,mpg01.test)

(6+6)/78 # 15% error rate 
```

```
## [1] 0.1538462
```

```r
# k = 3
# set.seed(1) # why set.seed() ?
# knn.pred.4=knn(train.2,test.2,mpg01.train ,k=3)
# table(knn.pred.4,mpg01.test)

(3+6)/78 # 11.5% error rate 
```

```
## [1] 0.1153846
```

```r
# k = 5 
# set.seed(1) # why set.seed() ?
# knn.pred.4=knn(train.2,test.2,mpg01.train ,k=5)
# table(knn.pred.4,mpg01.test)

(4+7)/78 # 14% error rate 
```

```
## [1] 0.1410256
```

```r
# should be able to draw a plot using k as x-axis and error rate as y-axis to see how error rate changes with a increased k. 
```

### 13. 

Using the Boston data set, fit classification models in order to predict whether a given suburb has a crime rate above or below the median. Explore logistic regression, LDA, and KNN models using various subsets of the predictors. Describe your findings. 


```r
colnames(Boston)
```

```
##  [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
##  [8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"
```

```r
Boston2 <- 
  Boston %>% 
  mutate(crim01 = as.factor(ifelse(crim > median(crim), 1, 0))) %>% 
  dplyr::select(-crim)

colnames(Boston2)  
```

```
##  [1] "zn"      "indus"   "chas"    "nox"     "rm"      "age"     "dis"    
##  [8] "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"    "crim01"
```

```r
# get training & test dataset 
set.seed(1)
train_ID <- sample(rownames(Boston2), size = round(nrow(Auto) * 0.8), replace = F) 
train <- Boston2[rownames(Boston2) %in% train_ID,]
test <- Boston2[!(rownames(Boston2) %in% train_ID),] 

dim(train) # 314
```

```
## [1] 314  14
```

```r
dim(test) # 192
```

```
## [1] 192  14
```

```r
# logsitic, full model to test which one is significant 
attach(Boston2)

glm.fits.1 <- glm(crim01 ~ ., data = train, family=binomial)
glm.fits.1 %>% summary() # nox, age, dis, and rad are important 
```

```
## 
## Call:
## glm(formula = crim01 ~ ., family = binomial, data = train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7354  -0.1179   0.0001   0.0118   3.5995  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -41.013303   8.874994  -4.621 3.81e-06 ***
## zn           -0.064062   0.042642  -1.502  0.13301    
## indus        -0.115014   0.062837  -1.830  0.06720 .  
## chas          0.046833   0.989146   0.047  0.96224    
## nox          55.370843  10.752839   5.149 2.61e-07 ***
## rm           -0.505948   0.927374  -0.546  0.58536    
## age           0.055717   0.018892   2.949  0.00319 ** 
## dis           0.924925   0.323685   2.857  0.00427 ** 
## rad           0.497643   0.193568   2.571  0.01014 *  
## tax          -0.007083   0.004280  -1.655  0.09795 .  
## ptratio       0.317690   0.163721   1.940  0.05233 .  
## black        -0.006093   0.005364  -1.136  0.25600    
## lstat         0.032356   0.069873   0.463  0.64332    
## medv          0.169274   0.090525   1.870  0.06150 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 434.84  on 313  degrees of freedom
## Residual deviance: 120.52  on 300  degrees of freedom
## AIC: 148.52
## 
## Number of Fisher Scoring iterations: 9
```

```r
# make prediction only using the several imporant predictors 
glm.fits <- glm(crim01 ~ nox + age + dis + rad, data = train, family=binomial)
glm.probs=predict(glm.fits,test,type="response")

glm.pred=rep(0,192)
glm.pred[glm.probs >.5]=1 # need to understand these more... 
table(glm.pred,test$crim01)
```

```
##         
## glm.pred  0  1
##        0 89 21
##        1 13 69
```

```r
(21+13)/(192) # 17.7% test error rate 
```

```
## [1] 0.1770833
```

```r
# LDA
lda.fit=lda(crim01 ~ nox + age + dis + rad,data=train)
lda.fit
```

```
## Call:
## lda(crim01 ~ nox + age + dis + rad, data = train)
## 
## Prior probabilities of groups:
##         0         1 
## 0.4808917 0.5191083 
## 
## Group means:
##         nox      age      dis       rad
## 0 0.4699245 52.21523 4.962823  4.456954
## 1 0.6427239 86.98221 2.494895 15.693252
## 
## Coefficients of linear discriminants:
##             LD1
## nox 6.944663881
## age 0.018086158
## dis 0.006082187
## rad 0.064443772
```

```r
lda.pred=predict(lda.fit, test)

lda.class=lda.pred$class
table(lda.class ,test$crim01)
```

```
##          
## lda.class  0  1
##         0 95 25
##         1  7 65
```

```r
(25+7)/192 # 16.7 
```

```
## [1] 0.1666667
```

```r
# KNN 
# opps, forget to standadize data in the previous Q ... 
```

