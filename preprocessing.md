---
title: "Pre-processing"
author: "Nick Stern"
date: "4/28/2019"
output: 
  html_document:
    theme: flatly
    highlight: tango
    toc: true
    toc_float: true
    fig_width: 6
    fig_height: 4
    keep_md: true
---



This markdown files cleans the Acute Myocardial Infraction (AMI) dataset, taken from the Canvas course page. The step by step pre-processing actions are outlined below.

First we read in the dataset


```r
## Reading in the original data
library(dplyr)
ami = read.csv('data/amidata.csv')
head(ami)
```

```
##   Patient DIAGNOSIS SEX DRG DIED CHARGES LOS AGE
## 1       1     41041   F 122    0    4752  10  79
## 2       2     41041   F 122    0    3941   6  34
## 3       3     41091   F 122    0    3657   5  76
## 4       4     41081   F 122    0    1481   2  80
## 5       5     41091   M 122    0    1681   1  55
## 6       6     41091   M 121    0    6379   9  84
```

Then we made the variable names lowercase for ease of use and to adhere to convention.


```r
# make variable names lowercase
ami.names<-tolower(colnames(ami))
colnames(ami)<-ami.names
```

Then we imputed the missing values in the ```charges``` column with the ```na.convert.mean``` function from lecture notes. This function imputes missing values with the mean of the predictor, and creates a new binary predictor that is essentially an indicator variable for which values were originally missing. 


```r
source('na-convert.R')  # NA conversion function from class
# impute missing values
ami <- na.convert.mean(ami)
```

Finally, we factorized some of the categorical variables that weren't factorized by default.


```r
#recode variables
ami <- ami %>%
  mutate(diagnosis = as.factor(diagnosis),
         drg = as.factor(drg),
         died = as.factor(died))
```

Below we provide a summary of the pre-processed dataset, and save it to the local filesystem.


```r
#summary statistics
summary(ami)
```

```
##     patient        diagnosis    sex       drg       died     
##  Min.   :    1   41091  :5213   F:5065   121:5387   0:11434  
##  1st Qu.: 3212   41041  :2665   M:7779   122:6047   1: 1410  
##  Median : 6422   41011  :1824            123:1410            
##  Mean   : 6422   41071  :1703                                
##  3rd Qu.: 9633   41001  : 467                                
##  Max.   :12844   41081  : 287                                
##                  (Other): 685                                
##     charges           los              age           charges.na     
##  Min.   :    3   Min.   : 0.000   Min.   : 20.00   Min.   :0.00000  
##  1st Qu.: 5606   1st Qu.: 4.000   1st Qu.: 57.00   1st Qu.:0.00000  
##  Median : 8816   Median : 7.000   Median : 67.00   Median :0.00000  
##  Mean   : 9879   Mean   : 7.569   Mean   : 66.29   Mean   :0.05442  
##  3rd Qu.:12254   3rd Qu.:10.000   3rd Qu.: 77.00   3rd Qu.:0.00000  
##  Max.   :47910   Max.   :38.000   Max.   :103.00   Max.   :1.00000  
## 
```

```r
write.csv(file='data/amidata_cleaned.csv', x=ami)
```




