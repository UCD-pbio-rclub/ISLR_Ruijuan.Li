---
title: "unsupervised_learning"
author: "Ruijuan Li"
date: "5/17/2018"
output: 
  html_document: 
    keep_md: yes
---

### Q6

A researcher collects expression measurements for 1,000 genes in 100 tissue samples. The data can be written as a 1,000 × 100 matrix, which we call X, in which each row represents a gene and each column a tissue sample. Each tissue sample was processed on a different day, and the columns of X are ordered so that the samples that were processed earliest are on the left, and the samples that were processed later are on the right. The tissue samples belong to two groups: control (C) and treatment (T). The C and T samples were processed in a random order across the days. The researcher wishes to determine whether each gene’s expression measurements differ between the treatment and control groups.
As a pre-analysis (before comparing T versus C), the researcher performs a principal component analysis of the data, and finds that the first principal component (a vector of length 100) has a strong linear trend from left to right, and explains 10% of the variation. The researcher now remembers that each patient sample was run on one of two machines, A and B, and machine A was used more often in the earlier times while B was used more often later. The researcher has a record of which sample was run on which machine.

(a) Explain what it means that the first principal component “explains 10 % of the variation”.

10% of the tissue sample variation was due to the 1st PC, which comes from expression level differences among difference genes.  

(b) The researcher decides to replace the (j, i)th element of X with xji − φj1zi1 
where zi1 is the ith score, and φj1 is the jth loading, for the first principal component. He will then perform a two-sample t-test on each gene in this new data set in order to determine whether its expression differs between the two conditions. Critique this idea, and suggest a better approach. (The principal component analysis is performed on XT ).

By replacing the original data with PCA derived data, he wanted to decompose the time and machine effect so that he can compare T and C. There are two problems, 1) the way he did it extracted variations due to genes, instead of variation due to time or machine 2) multiple test problem 

I would use edgeR's negative binomial model for this problem, model: expression ~ treatment * machine + time, look for genes significant for treatment effect and then use FDR to account for multiple test problem. 

(c) Design and run a small simulation experiment to demonstrate the superiority of your idea.

skip for now... 

### Q8

In Section 10.2.3, a formula for calculating PVE was given in Equation 10.8. We also saw that the PVE can be obtained using the sdev output of the prcomp() function.

On the USArrests data, calculate PVE in two ways:

(a) Using the sdev output of the prcomp() function, as was done in Section 10.2.3.


```r
names(USArrests)
```

```
## [1] "Murder"   "Assault"  "UrbanPop" "Rape"
```

```r
pr.out=prcomp(USArrests, scale=TRUE)
pr.out$sdev
```

```
## [1] 1.5748783 0.9948694 0.5971291 0.4164494
```

```r
pr.var=pr.out$sdev ^2 
pve=pr.var/sum(pr.var)
pve 
```

```
## [1] 0.62006039 0.24744129 0.08914080 0.04335752
```

(b) By applying Equation 10.8 directly. That is, use the prcomp() function to compute the principal component loadings. Then, use those loadings in Equation 10.8 to obtain the PVE.



These two approaches should give the same results.

Hint: You will only obtain the same results in (a) and (b) if the same data is used in both cases. For instance, if in (a) you performed prcomp() using centered and scaled variables, then you must center and scale the variables before applying Equation 10.3 in (b).

### Q10

In this problem, you will generate simulated data, and then perform PCA and K-means clustering on the data.

(a) Generate a simulated data set with 20 observations in each of three classes (i.e. 60 observations total), and 50 variables.

Hint: There are a number of functions in R that you can use to generate data. One example is the rnorm() function; runif() is another option. Be sure to add a mean shift to the observations in each class so that there are three distinct classes.


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
data <- 
sapply(1:50, function(i) c(rnorm(20, mean = 10, sd = 1), rnorm(20, mean = 20, sd = 2), rnorm(20, mean = 30, sd = 3))) %>% as.data.frame() 
data[1:10, 1:10]
```

```
##           V1        V2        V3        V4        V5        V6        V7
## 1  10.876456 11.419363  8.589446  9.671994  9.259126  9.732808 10.226653
## 2  11.201086 10.294743  9.521496 11.254787  9.373144  9.108095  9.544754
## 3  12.406639  8.854611  9.876370  9.399606 10.291142 10.745400 10.864544
## 4   9.620806  9.005791  9.778290  8.816137  9.911920  9.604934  9.655900
## 5   9.384060  9.872619 10.635269  8.674670  9.487256 10.041939  9.764265
## 6   9.814201  9.432681 11.081316 11.184754 11.297163  9.519394  9.599182
## 7   9.975234 10.125340 11.672837 10.226252  9.052306  9.908554 10.241894
## 8  10.280378  8.778324  8.896747 11.415923 11.061765  9.633419  8.513665
## 9  10.246792 10.247617  9.442484  8.900886 12.472660  9.731657  9.598588
## 10 10.904710 11.233063  9.562966  9.906183 10.472166 10.442881 11.649771
##           V8        V9       V10
## 1  11.055944  8.593083 11.404961
## 2  10.706212  9.648369 10.454962
## 3  10.069172  9.647675  9.539345
## 4  10.002200 11.589613 11.269596
## 5  10.135269  9.967422  7.857938
## 6   9.417160 11.693482 10.005499
## 7  11.496477 11.350397 10.872358
## 8  10.533903  8.765182 10.708317
## 9   9.653318 11.171807  9.212739
## 10  8.358479  8.267543 12.010857
```

```r
dim(data) 
```

```
## [1] 60 50
```

(b) Perform PCA on the 60 observations and plot the first two principal component score vectors. Use a different color to indicate the observations in each of the three classes. If the three classes appear separated in this plot, then continue on to part (c). If not, then return to part (a) and modify the simulation so that there is greater separation between the three classes. Do not continue to part (c) until the three classes show at least some separation in the first two principal component score vectors.


```r
# pr.out=prcomp(data, scale=TRUE)  
# pc <- 
# pr.out$rotation %>% as.data.frame() %>% 
#   dplyr::select(PC1, PC2)
#   
# data$class[1:20] <- rep("class1", 20)
# data$class[21:40] <- rep("class2", 20)
# data$class[41:60] <- rep("class3", 20)
# 
# pc$class <- data$class 
# 
# pc %>% 
#   ggplot() +
#   geom_point(aes(x = PC1, y = PC2, color = class))   
```

 
