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

### Q9 
Consider the USArrests data. We will now perform hierarchical clustering on the states.

(a) Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.


```r
hc.complete=hclust(dist(USArrests), method="complete")
```

(b) Cut the dendrogram at a height that results in three distinct clusters. Which states belong to which clusters?


```r
cutree(hc.complete, 3)
```

```
##        Alabama         Alaska        Arizona       Arkansas     California 
##              1              1              1              2              1 
##       Colorado    Connecticut       Delaware        Florida        Georgia 
##              2              3              1              1              2 
##         Hawaii          Idaho       Illinois        Indiana           Iowa 
##              3              3              1              3              3 
##         Kansas       Kentucky      Louisiana          Maine       Maryland 
##              3              3              1              3              1 
##  Massachusetts       Michigan      Minnesota    Mississippi       Missouri 
##              2              1              3              1              2 
##        Montana       Nebraska         Nevada  New Hampshire     New Jersey 
##              3              3              1              3              2 
##     New Mexico       New York North Carolina   North Dakota           Ohio 
##              1              1              1              3              3 
##       Oklahoma         Oregon   Pennsylvania   Rhode Island South Carolina 
##              2              2              3              2              1 
##   South Dakota      Tennessee          Texas           Utah        Vermont 
##              3              2              2              3              3 
##       Virginia     Washington  West Virginia      Wisconsin        Wyoming 
##              2              2              3              3              2
```

(c) Hierarchically cluster the states using complete linkage and Euclidean distance, after scaling the variables to have standard deviation one.


```r
USArrests.scale=scale(USArrests)
hc.complete.scale=hclust(dist(USArrests.scale), method="complete")
cutree(hc.complete.scale, 4)
```

```
##        Alabama         Alaska        Arizona       Arkansas     California 
##              1              1              2              3              2 
##       Colorado    Connecticut       Delaware        Florida        Georgia 
##              2              3              3              2              1 
##         Hawaii          Idaho       Illinois        Indiana           Iowa 
##              3              4              2              3              4 
##         Kansas       Kentucky      Louisiana          Maine       Maryland 
##              3              3              1              4              2 
##  Massachusetts       Michigan      Minnesota    Mississippi       Missouri 
##              3              2              3              1              3 
##        Montana       Nebraska         Nevada  New Hampshire     New Jersey 
##              4              4              2              4              3 
##     New Mexico       New York North Carolina   North Dakota           Ohio 
##              2              2              1              4              3 
##       Oklahoma         Oregon   Pennsylvania   Rhode Island South Carolina 
##              3              3              3              3              1 
##   South Dakota      Tennessee          Texas           Utah        Vermont 
##              4              1              2              3              4 
##       Virginia     Washington  West Virginia      Wisconsin        Wyoming 
##              3              3              4              3              3
```

(d) What effect does scaling the variables have on the hierarchical clustering obtained? In your opinion, should the variables be scaled before the inter-observation dissimilarities are computed? Provide a justification for your answer.


```r
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 3.2.5
```

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
par(mfrow=c(1,2))
plot(hclust(dist(USArrests), method="complete"), main = "before scaling")
plot(hclust(dist(USArrests.scale), method="complete"), main = "after scaling")
```

![](unsupervised_learning_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# before scaling, looks like 3 major branches formed, after scaline, looks like four major branches formed. I think it should be scaled, because otherwise assult will be have a large effect on the final result compared to other variables. 
summary(USArrests)
```

```
##      Murder          Assault         UrbanPop          Rape      
##  Min.   : 0.800   Min.   : 45.0   Min.   :32.00   Min.   : 7.30  
##  1st Qu.: 4.075   1st Qu.:109.0   1st Qu.:54.50   1st Qu.:15.07  
##  Median : 7.250   Median :159.0   Median :66.00   Median :20.10  
##  Mean   : 7.788   Mean   :170.8   Mean   :65.54   Mean   :21.23  
##  3rd Qu.:11.250   3rd Qu.:249.0   3rd Qu.:77.75   3rd Qu.:26.18  
##  Max.   :17.400   Max.   :337.0   Max.   :91.00   Max.   :46.00
```

```r
USArrests$state <- rownames(USArrests)
USArrests  %>% 
  melt() %>% 
  ggplot() + 
  geom_bar(aes(x = variable, y = value, fill = state), stat = "identity", position=position_dodge())
```

```
## Using state as id variables
```

![](unsupervised_learning_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

### Q10

In this problem, you will generate simulated data, and then perform PCA and K-means clustering on the data.

(a) Generate a simulated data set with 20 observations in each of three classes (i.e. 60 observations total), and 50 variables.

Hint: There are a number of functions in R that you can use to generate data. One example is the rnorm() function; runif() is another option. Be sure to add a mean shift to the observations in each class so that there are three distinct classes.


```r
data <- 
sapply(1:50, function(i) c(rnorm(20, mean = 10, sd = 1), rnorm(20, mean = 20, sd = 2), rnorm(20, mean = 30, sd = 3))) %>% as.data.frame() 
data[1:10, 1:10]
```

```
##           V1        V2        V3        V4        V5        V6        V7
## 1   9.589302  8.970347 11.172887 11.186853  9.850464 11.542896  8.017192
## 2  10.385725 10.424276 10.951487  9.114210  7.165597 10.837531 11.807783
## 3  11.848115 11.496619 10.141525  8.656001  9.533460  9.435331 10.959418
## 4  10.807997  8.582351  9.765168 10.429123 11.342261 10.698461 11.672164
## 5   8.655952 11.058781  9.760349  9.536057  9.724885 10.637398  9.342782
## 6   9.653930 10.729790 10.153190 10.056465 10.818928 10.197914 11.330315
## 7  10.181714  9.262705 10.151663  9.821808  9.214154 10.217819 11.189251
## 8   9.481232  9.217340  9.851262 10.127930  9.256361  9.647203 10.687896
## 9   9.647452  9.328369  9.813744  9.672215 10.707650 10.022098 11.173076
## 10 11.239666  8.919265 11.522655  7.834126  9.385411  9.391569  9.541878
##           V8        V9       V10
## 1  10.216178 10.383525  9.292078
## 2  11.781957 10.783668  9.624757
## 3   9.336239  9.646731  9.344548
## 4   9.908482 11.033776  9.047992
## 5  10.254162 10.157574 10.071559
## 6   9.138180  9.697743  9.942444
## 7  10.517811 10.309555  9.896389
## 8  10.880082 10.919785 10.660353
## 9  10.629734 11.197799  8.665386
## 10 11.621697  9.258033 12.406572
```

```r
dim(data) 
```

```
## [1] 60 50
```

(b) Perform PCA on the 60 observations and plot the first two principal component score vectors. Use a different color to indicate the observations in each of the three classes. If the three classes appear separated in this plot, then continue on to part (c). If not, then return to part (a) and modify the simulation so that there is greater separation between the three classes. Do not continue to part (c) until the three classes show at least some separation in the first two principal component score vectors.


```r
pr.out=prcomp(data, scale=TRUE)
pc <-
pr.out$x %>% as.data.frame() %>%
  dplyr::select(PC1, PC2)

data$class[1:20] <- rep("class1", 20)
data$class[21:40] <- rep("class2", 20)
data$class[41:60] <- rep("class3", 20)

pc$class <- data$class

pc %>%
  ggplot() +
  geom_point(aes(x = PC1, y = PC2, color = class))
```

![](unsupervised_learning_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

(c) Perform K-means clustering of the observations with K = 3. How well do the clusters that you obtained in K-means clustering compare to the true class labels?

Hint: You can use the table() function in R to compare the true class labels to the class labels obtained by clustering. Be careful how you interpret the results: K-means clustering will arbitrarily number the clusters, so you cannot simply check whether the true class labels and clustering labels are the same.


```r
km.out <- 
data %>% 
  dplyr::select(-class) %>% 
  kmeans(3,nstart=20) 

km.out$cluster 
```

```
##  [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [36] 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
```

```r
table(km.out$cluster, data$class) 
```

```
##    
##     class1 class2 class3
##   1      0     20      0
##   2      0      0     20
##   3     20      0      0
```

(d) Perform K-means clustering with K = 2. Describe your results.


```r
km.out <- 
data %>% 
  dplyr::select(-class) %>% 
  kmeans(2,nstart=20) 

km.out$cluster 
```

```
##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
## [36] 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
```

```r
table(km.out$cluster, data$class) 
```

```
##    
##     class1 class2 class3
##   1      0      0     20
##   2     20     20      0
```

```r
# class 1 and 2 are more similar 
```

(e) Now perform K-means clustering with K = 4, and describe your results.


```r
km.out <- 
data %>% 
  dplyr::select(-class) %>% 
  kmeans(4,nstart=20) 

km.out$cluster 
```

```
##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [36] 1 1 1 1 1 3 4 3 3 4 4 3 3 4 3 4 3 4 4 4 3 3 3 3 4
```

```r
table(km.out$cluster, data$class) 
```

```
##    
##     class1 class2 class3
##   1      0     20      0
##   2     20      0      0
##   3      0      0     11
##   4      0      0      9
```

```r
# class 3 were split to two classes  
```

(f) Now perform K-means clustering with K = 3 on the first two principal component score vectors, rather than on the raw data. That is, perform K-means clustering on the 60 × 2 matrix of which the first column is the first principal component score vector, and the second column is the second principal component score vector. Comment on the results.


```r
km.out <- 
pc %>% 
  dplyr::select(-class) %>% 
  kmeans(3,nstart=20) 

km.out$cluster 
```

```
##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [36] 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
```

```r
table(km.out$cluster, data$class) 
```

```
##    
##     class1 class2 class3
##   1      0     20      0
##   2     20      0      0
##   3      0      0     20
```

```r
# same result as using the raw data 
```

(g) Using the scale() function, perform K-means clustering with K = 3 on the data after scaling each variable to have standard deviation one. How do these results compare to those obtained in (b)? Explain.


```r
km.out <- 
data %>% 
  dplyr::select(-class) %>% 
  scale() %>% 
  kmeans(3,nstart=20) 

km.out$cluster 
```

```
##  [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
## [36] 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
```

```r
table(km.out$cluster, data$class) 
```

```
##    
##     class1 class2 class3
##   1      0      0     20
##   2      0     20      0
##   3     20      0      0
```

```r
# same as (b)   
```

### Q11

On the book website, www.StatLearning.com, there is a gene expression data set (Ch10Ex11.csv) that consists of 40 tissue samples with measurements on 1,000 genes. The first 20 samples are from healthy patients, while the second 20 are from a diseased group.

(a) Load in the data using read.csv(). You will need to select header=F.


```r
Ex11 <- read.csv("~/Desktop/2018_spring/ISLR_Ruijuan.Li/chapter_10/Ch10Ex11.csv", header = F)
Ex11 %>% dim()
```

```
## [1] 1000   40
```

(b) Apply hierarchical clustering to the samples using correlation- based distance, and plot the dendrogram. Do the genes separate the samples into the two groups? Do your results depend on the type of linkage used?


```r
hc.complete=hclust(as.dist(cor(Ex11)), method="complete")
hc.average=hclust(as.dist(cor(Ex11)), method="average") 
hc.single=hclust(as.dist(cor(Ex11)), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="",
cex =.9)
plot(hc.average , main="Average Linkage", xlab="", sub="",
cex =.9)
plot(hc.single , main="Single Linkage", xlab="", sub="",
cex =.9)
```

![](unsupervised_learning_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
# yes, linkage has a big effect

# if scale 
Ex11.scale <- scale(Ex11)
hc.complete=hclust(as.dist(cor(Ex11.scale)), method="complete")
hc.average=hclust(as.dist(cor(Ex11.scale)), method="average") 
hc.single=hclust(as.dist(cor(Ex11.scale)), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="",
cex =.9)
plot(hc.average , main="Average Linkage", xlab="", sub="",
cex =.9)
plot(hc.single , main="Single Linkage", xlab="", sub="",
cex =.9)
```

![](unsupervised_learning_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

```r
# compared to unscaled data, looks like the same result, so scale doesn't have effect on correlation based clustering.
```

(c) Your collaborator wants to know which genes differ the most across the two groups. Suggest a way to answer this question, and apply it here.


```r
Ex11 %>% head()
```

```
##            V1         V2         V3         V4         V5         V6
## 1 -0.96193340  0.4418028 -0.9750051  1.4175040  0.8188148  0.3162937
## 2 -0.29252570 -1.1392670  0.1958370 -1.2811210 -0.2514393  2.5119970
## 3  0.25878820 -0.9728448  0.5884858 -0.8002581 -1.8203980 -2.0589240
## 4 -1.15213200 -2.2131680 -0.8615249  0.6309253  0.9517719 -1.1657240
## 5  0.19578280  0.5933059  0.2829921  0.2471472  1.9786680 -0.8710180
## 6  0.03012394 -0.6910143 -0.4034258 -0.7298590 -0.3640986  1.1253490
##            V7          V8          V9        V10        V11        V12
## 1 -0.02496682 -0.06396600  0.03149702 -0.3503106 -0.7227299 -0.2819547
## 2 -0.92220620  0.05954277 -1.40964500 -0.6567122 -0.1157652  0.8259783
## 3 -0.06476437  1.59212400 -0.17311700 -0.1210874 -0.1875790 -1.5001630
## 4 -0.39155860  1.06361900 -0.35000900 -1.4890580 -0.2432189 -0.4330340
## 5 -0.98971500 -1.03225300 -1.10965400 -0.3851423  1.6509570 -1.7449090
## 6 -1.40404100 -0.80613040 -1.23792400  0.5776018 -0.2720642  2.1765620
##           V13         V14        V15        V16        V17        V18
## 1  1.33751500  0.70197980  1.0076160 -0.4653828  0.6385951  0.2867807
## 2  0.34644960 -0.56954860 -0.1315365  0.6902290 -0.9090382  1.3026420
## 3 -1.22873700  0.85598900  1.2498550 -0.8980815  0.8702058 -0.2252529
## 4 -0.03879128 -0.05789677 -1.3977620 -0.1561871 -2.7359820  0.7756169
## 5 -0.37888530 -0.67982610 -2.1315840 -0.2301718  0.4661243 -1.8004490
## 6  1.43640700 -1.02578100  0.2981582 -0.5559659  0.2046529 -1.1916480
##          V19         V20        V21        V22        V23        V24
## 1 -0.2270782 -0.22004520 -1.2425730 -0.1085056 -1.8642620 -0.5005122
## 2 -1.6726950 -0.52550400  0.7979700 -0.6897930  0.8995305  0.4285812
## 3  0.4502892  0.55144040  0.1462943  0.1297400  1.3042290 -1.6619080
## 4  0.6141562  2.01919400  1.0811390 -1.0766180 -0.2434181  0.5134822
## 5  0.6262904 -0.09772305 -0.2997108 -0.5295591 -2.0235670 -0.5108402
## 6  0.2350916  0.67096470  0.1307988  1.0689940  1.2309870  1.1344690
##           V25         V26        V27        V28         V29         V30
## 1 -1.32500800  1.06341100 -0.2963712 -0.1216457  0.08516605  0.62417640
## 2 -0.67611410 -0.53409490 -1.7325070 -1.6034470 -1.08362000  0.03342185
## 3 -1.63037600 -0.07742528  1.3061820  0.7926002  1.55946500 -0.68851160
## 4 -0.51285780  2.55167600 -2.3143010 -1.2764700 -1.22927100  1.43439600
## 5  0.04600274  1.26803000 -0.7439868  0.2231319  0.85846280  0.27472610
## 6  0.55636800 -0.35876640  1.0798650 -0.2064905 -0.00616453  0.16425470
##          V31          V32         V33        V34        V35        V36
## 1 -0.5095915 -0.216725500 -0.05550597 -0.4844491 -0.5215811  1.9491350
## 2  1.7007080  0.007289556  0.09906234  0.5638533 -0.2572752 -0.5817805
## 3 -0.6154720  0.009999363  0.94581000 -0.3185212 -0.1178895  0.6213662
## 4 -0.2842774  0.198945600 -0.09183320  0.3496279 -0.2989097  1.5136960
## 5 -0.6929984 -0.845707200 -0.17749680 -0.1664908  1.4831550 -1.6879460
## 6  1.1567370  0.241774500  0.08863952  0.1829540  0.9426771 -0.2096004
##           V37        V38         V39        V40
## 1  1.32433500  0.4681471  1.06110000  1.6559700
## 2 -0.16988710 -0.5423036  0.31293890 -1.2843770
## 3 -0.07076396  0.4016818 -0.01622713 -0.5265532
## 4  0.67118470  0.0108553 -1.04368900  1.6252750
## 5 -0.14142960  0.2007785 -0.67594210  2.2206110
## 6  0.53626210 -1.1852260 -0.42274760  0.6243603
```


