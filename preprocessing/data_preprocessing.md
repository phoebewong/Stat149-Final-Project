---
title: "Data Preprocessing"
author: "Karina Huang"
date: "4/30/2019"
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



# Exploratory Data Analysis 

In accordance with the desciption provided, the current dataset consist of 12,844 observations and 8 variable columns. The variable types are:

  * Patient: integer, patient ID, numbered 1 to 12,844 (DISCRETE, EXCLUDE FROM MODEL)
  * DIAGNOSIS: integer, 5-digit code indicating the part of heart that was affected (RECODE: CATEGORICAL)
  * SEX: factor, F for female, M for male (CATEGORICAL)
  * DRG: integer, code indicating the Diagnosis Related Group (RECODE: CATEGORICAL)
  * DIED: integer, 1 for patients who died in the hospital, 0 otherwise (REMOVE: REDUNDANT)
  * CHARGES: integer, total hospital charges in US dollars (CONTINUOUS)
  * LOS: integer, hospital length of stay in days since admittance (RESPONSE: QUANTITATIVE, DISCRETE/CONTINUOUS)
  * AGE: integer, age of the patient in years (RESPONSE: QUANTITATVE, DISCRETE/CONTINUOUS)
  

```r
#load dataset
ami <- read.csv('data/amidata.csv')
#check missing values
colSums(is.na(ami))
```

```
##   Patient DIAGNOSIS       SEX       DRG      DIED   CHARGES       LOS 
##         0         0         0         0         0       699         0 
##       AGE 
##         0
```

There are 699 missing values from the variable "charges," we used the `na.convert.mean` function from lecture notes to impute the missing values with the mean and contruct and extra column to test the contribution of the missing values to this variable.




```r
#turn variable names to lowercase
ami.names<-tolower(colnames(ami))
colnames(ami)<-ami.names
#impute missing values
ami <- na.convert.mean(ami)
#recode variables
ami <- ami[, !(names(ami) == 'died')] %>%
  mutate(diagnosis = as.factor(diagnosis),
         drg = as.factor(drg)) 
#summary statistics
summary(ami)
```

```
##     patient        diagnosis    sex       drg          charges     
##  Min.   :    1   41091  :5213   F:5065   121:5387   Min.   :    3  
##  1st Qu.: 3212   41041  :2665   M:7779   122:6047   1st Qu.: 5606  
##  Median : 6422   41011  :1824            123:1410   Median : 8816  
##  Mean   : 6422   41071  :1703                       Mean   : 9879  
##  3rd Qu.: 9633   41001  : 467                       3rd Qu.:12254  
##  Max.   :12844   41081  : 287                       Max.   :47910  
##                  (Other): 685                                      
##       los              age           charges.na     
##  Min.   : 0.000   Min.   : 20.00   Min.   :0.00000  
##  1st Qu.: 4.000   1st Qu.: 57.00   1st Qu.:0.00000  
##  Median : 7.000   Median : 67.00   Median :0.00000  
##  Mean   : 7.569   Mean   : 66.29   Mean   :0.05442  
##  3rd Qu.:10.000   3rd Qu.: 77.00   3rd Qu.:0.00000  
##  Max.   :38.000   Max.   :103.00   Max.   :1.00000  
## 
```

```r
#save data into rds
saveRDS(ami, file = 'amidata_cleaned.rds')
```


  
