---
title: "Gamma"
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



# Abstract

This R markdown file serves to fit a Guassian model to the response data. The motivation for this is because the response variable exhibits right-skewness. See the exploratory data analysis notebook for visuals in support of this claim.

# Fitting the Gamma GLM

### Discussion of zero-valued observation

Before we can fit the Gamma GLM, we need to deal with the one observation that has a response value of zero, as the Gamma distribution is defined for all positive continuous numbers. Note that our response variable is discrete, but time itself is a continuous metric, and we can think of the response variable as an aggregated form of a continuous latent variable. We first examined the details of the zero-valued observation to see if there was anything awry. 



```r
ami = readRDS("../../data/amidata_clean.rds")
ami[ami$los == 0, ]
```

```
##      patient diagnosis sex drg died charges los age charges.na
## 9159    9159     41091   F 122    0   10366   0  53          0
```

```r
table(ami$los)[1:15]
```

```
## 
##    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14 
##    1 1194  732  750  826  980 1303 1418 1262 1040  742  581  407  330  254
```

From the information about the zero-valued datapoint, we do not see anything that is unbelievable or any feature that would insinuate this point is an outlier besides the length of stay. Without information on how the data were collected, we can't know apriori whether this datapoint was a result of data misentry, a violation of hospital policies, or a medical miracle. We can, however, compare the distribution of length of stay for patients with the same diagnosis with the overall population to see if perhaps there is a discrepancy between groups.



```r
# Examination of diagnosis
library(ggplot2)
ggplot(data=ami, aes(x=los)) +
    geom_histogram(data=subset(ami, diagnosis == 41091), aes(fill='41091'), alpha = 0.2) +
    geom_histogram(alpha = 0.2, aes(fill='All')) + 
    scale_fill_manual(name="Diagnosis", values=c('red', 'blue'))
```

![](gamma_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The results indicate that the diagnosis is representative of the overall population, and therefore the mixed effects hypothesis is not a valid reason for why we are seeing an outlier. This makes sense contextually since the diagnosis code 41091 represents "Acute myocardial infarction of unspecified site, initial episode of care - as a primary diagnosis." The "unspecified site" likely means it is a catchall diagnosis and spans a wide range of severity.

We can also look at the length of stay for the patients who died to see if there is a peak at 1 day. Our suspicion is that of the patients who died, a considerable number of them would have died the same day, as survival is extremely short-term when the heart stops beating. 


```r
hist(subset(ami$los, ami$died == 1), breaks=40, 
     xlab='Length of Stay', main='Histogram of Length of Stay Given Patient Died')
```

![](gamma_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

We do see a considerable peak at 1 day. This observation is convincing, but Professor Glickman pointed out that if the length of stay is based around the completion of paperwork, it is possible that patients who die the same day do not have their paperwork completed until the next. 

Taking everything into consideration, we are left with a few options:

1. Remove the datapoint under the assumption that it was incorrectly entered, using the length of stay distribution for those who died as evidence that same-day discharge is equivalent to a length of stay of 1.

2. Remove the datapoint under the assumption that it was correctly entered, effectively imposing a floor on our data and reflecting that in the model inference.

3. Shift all the response values by +1 under the assumption that the datapoint is accurate, and there is one person who was discharged and/or had their paperwork completed on the same day. Adjust the model interpretation correspondingly.

4. Change the datapoint to be 1, under the assumption that the data was generated using some kind of rounding to the nearest day, and to coincide with what we would likely do with our Gamma model's predictions that fall below 0.5. 

We also considered the option of fitting a hurdle model to the data, where the zero population would come from a binomial distribution and the rest of the data would come from a Gamma distribution. However, we determined it was not meaningful to fit a model parameter to a single datapoint, thereby electing to forgo this approach.

Ultimately, we chose to round the zero-valued datapoint to 1. This decision was motivated in large part by the mechanics of what would happen to a Gamma model prediction that was close to 0 (we would end up rounding this value to the nearest positive integer anyway), and also from the fact that removing a single, non-influential (none of the feature values were extreme) datatpoint from a dataset of more than 12,000 patients will have little to no effect on the predictions of our model. 


```r
ami$los[ami$los == 0] = 1
```

### Feature Selection

We chose our best fit model using stepwise feature selection. The stepwise regression uses an iterative forward/backward selection method and the AIC criterion to choose the best model. To get a baseline, we first ran stepwise feature selection without interaction terms. Note, we removed patient id since there should be no causal connection between the patient id and the length of stay if the data were appropriately randomized.


```r
# Without interaction terms
step(glm(los ~ .-patient, family=Gamma(log), data=ami), trace=0, direction='both')
```

```
## 
## Call:  glm(formula = los ~ diagnosis + sex + drg + charges + age + charges.na, 
##     family = Gamma(log), data = ami)
## 
## Coefficients:
##    (Intercept)  diagnosis41011  diagnosis41021  diagnosis41031  
##      7.370e-01       1.401e-02      -9.595e-03      -6.233e-02  
## diagnosis41041  diagnosis41051  diagnosis41071  diagnosis41081  
##      8.137e-03       1.112e-01       1.055e-02      -5.108e-02  
## diagnosis41091            sexM          drg122          drg123  
##      6.081e-02      -4.212e-02      -4.695e-02      -7.308e-01  
##        charges             age      charges.na  
##      7.055e-05       8.424e-03       1.497e-01  
## 
## Degrees of Freedom: 12843 Total (i.e. Null);  12829 Residual
## Null Deviance:	    6558 
## Residual Deviance: 3401 	AIC: 65410
```

The results indicate that the ```died``` feature did not improve the model fit to where it was warranted including it as an extra predictor. Next, we ran stepwise feature selection with all the interaction terms considered (of which there are many due to the multi-level factors in our dataset), to compare with our baseline.  


```r
# With interaction terms
step(glm(los ~ (.-patient)^2, family=Gamma(log), data=ami), trace=0, direction='both')
```

```
## 
## Call:  glm(formula = los ~ diagnosis + sex + drg + charges + age + charges.na + 
##     diagnosis:drg + diagnosis:charges + sex:age + sex:charges.na + 
##     drg:charges + drg:age + drg:charges.na + charges:age, family = Gamma(log), 
##     data = ami)
## 
## Coefficients:
##            (Intercept)          diagnosis41011          diagnosis41021  
##              5.424e-01               1.558e-01               8.425e-02  
##         diagnosis41031          diagnosis41041          diagnosis41051  
##             -1.659e-01              -7.330e-03               2.266e-03  
##         diagnosis41071          diagnosis41081          diagnosis41091  
##              3.286e-02              -1.103e-01               5.479e-02  
##                   sexM                  drg122                  drg123  
##              1.409e-01              -1.400e-01              -9.775e-01  
##                charges                     age              charges.na  
##              7.282e-05               1.321e-02               2.561e-01  
##  diagnosis41011:drg122   diagnosis41021:drg122   diagnosis41031:drg122  
##             -6.278e-02               2.673e-02               9.561e-02  
##  diagnosis41041:drg122   diagnosis41051:drg122   diagnosis41071:drg122  
##              4.197e-02               8.813e-02              -1.244e-02  
##  diagnosis41081:drg122   diagnosis41091:drg122   diagnosis41011:drg123  
##             -2.727e-02               3.663e-02              -2.220e-02  
##  diagnosis41021:drg123   diagnosis41031:drg123   diagnosis41041:drg123  
##             -2.581e-01              -1.000e-01               1.217e-01  
##  diagnosis41051:drg123   diagnosis41071:drg123   diagnosis41081:drg123  
##              3.649e-01               7.414e-02               1.294e-01  
##  diagnosis41091:drg123  diagnosis41011:charges  diagnosis41021:charges  
##              5.626e-02              -9.888e-06              -7.906e-06  
## diagnosis41031:charges  diagnosis41041:charges  diagnosis41051:charges  
##              5.815e-06              -1.090e-06               1.162e-06  
## diagnosis41071:charges  diagnosis41081:charges  diagnosis41091:charges  
##             -2.235e-06               6.341e-06              -1.286e-06  
##               sexM:age         sexM:charges.na          drg122:charges  
##             -2.660e-03              -9.147e-02               1.884e-05  
##         drg123:charges              drg122:age              drg123:age  
##              4.665e-05              -1.712e-03              -3.473e-03  
##      drg122:charges.na       drg123:charges.na             charges:age  
##             -1.566e-01              -6.514e-02              -2.009e-07  
## 
## Degrees of Freedom: 12843 Total (i.e. Null);  12796 Residual
## Null Deviance:	    6558 
## Residual Deviance: 3270 	AIC: 64960
```

As we can see from the step function output, the addition of a handful of interaction terms between ```diagnosis```, ```age```, ```charges```, ```drg```, and ```charges.na```. Next, we ran the model with these specified interaction to obtain the following fit.



```r
gamma_model = glm(formula = los ~ diagnosis + sex + drg + charges + age + charges.na + 
    diagnosis:drg + diagnosis:charges + sex:age + sex:charges.na + 
    drg:charges + drg:age + drg:charges.na + charges:age, family = Gamma(log), 
    data = ami)
summary(gamma_model)
```

```
## 
## Call:
## glm(formula = los ~ diagnosis + sex + drg + charges + age + charges.na + 
##     diagnosis:drg + diagnosis:charges + sex:age + sex:charges.na + 
##     drg:charges + drg:age + drg:charges.na + charges:age, family = Gamma(log), 
##     data = ami)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.5222  -0.3711  -0.0316   0.2310   4.8991  
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             5.424e-01  8.864e-02   6.119 9.70e-10 ***
## diagnosis41011          1.558e-01  6.412e-02   2.429 0.015146 *  
## diagnosis41021          8.425e-02  9.871e-02   0.853 0.393410    
## diagnosis41031         -1.659e-01  1.008e-01  -1.645 0.099972 .  
## diagnosis41041         -7.330e-03  6.273e-02  -0.117 0.906974    
## diagnosis41051          2.266e-03  1.216e-01   0.019 0.985128    
## diagnosis41071          3.286e-02  6.431e-02   0.511 0.609448    
## diagnosis41081         -1.103e-01  9.455e-02  -1.166 0.243438    
## diagnosis41091          5.479e-02  5.982e-02   0.916 0.359795    
## sexM                    1.409e-01  5.083e-02   2.773 0.005567 ** 
## drg122                 -1.400e-01  7.515e-02  -1.863 0.062528 .  
## drg123                 -9.775e-01  1.184e-01  -8.257  < 2e-16 ***
## charges                 7.282e-05  5.186e-06  14.040  < 2e-16 ***
## age                     1.321e-02  9.649e-04  13.690  < 2e-16 ***
## charges.na              2.561e-01  3.727e-02   6.871 6.69e-12 ***
## diagnosis41011:drg122  -6.278e-02  5.988e-02  -1.048 0.294486    
## diagnosis41021:drg122   2.673e-02  8.988e-02   0.297 0.766184    
## diagnosis41031:drg122   9.561e-02  8.640e-02   1.107 0.268480    
## diagnosis41041:drg122   4.197e-02  5.781e-02   0.726 0.467870    
## diagnosis41051:drg122   8.813e-02  1.062e-01   0.830 0.406539    
## diagnosis41071:drg122  -1.244e-02  6.002e-02  -0.207 0.835836    
## diagnosis41081:drg122  -2.727e-02  8.718e-02  -0.313 0.754482    
## diagnosis41091:drg122   3.663e-02  5.594e-02   0.655 0.512524    
## diagnosis41011:drg123  -2.220e-02  7.734e-02  -0.287 0.774043    
## diagnosis41021:drg123  -2.581e-01  1.257e-01  -2.054 0.040021 *  
## diagnosis41031:drg123  -1.000e-01  1.305e-01  -0.767 0.443347    
## diagnosis41041:drg123   1.217e-01  7.811e-02   1.559 0.119125    
## diagnosis41051:drg123   3.649e-01  1.399e-01   2.608 0.009122 ** 
## diagnosis41071:drg123   7.414e-02  8.855e-02   0.837 0.402475    
## diagnosis41081:drg123   1.294e-01  1.137e-01   1.139 0.254883    
## diagnosis41091:drg123   5.626e-02  7.168e-02   0.785 0.432542    
## diagnosis41011:charges -9.888e-06  3.867e-06  -2.557 0.010569 *  
## diagnosis41021:charges -7.906e-06  5.937e-06  -1.331 0.183055    
## diagnosis41031:charges  5.815e-06  6.563e-06   0.886 0.375634    
## diagnosis41041:charges -1.090e-06  3.847e-06  -0.283 0.776937    
## diagnosis41051:charges  1.162e-06  8.924e-06   0.130 0.896406    
## diagnosis41071:charges -2.235e-06  3.952e-06  -0.566 0.571636    
## diagnosis41081:charges  6.341e-06  5.821e-06   1.089 0.276059    
## diagnosis41091:charges -1.286e-06  3.652e-06  -0.352 0.724654    
## sexM:age               -2.660e-03  7.304e-04  -3.642 0.000272 ***
## sexM:charges.na        -9.147e-02  4.116e-02  -2.222 0.026291 *  
## drg122:charges          1.884e-05  1.706e-06  11.044  < 2e-16 ***
## drg123:charges          4.665e-05  2.074e-06  22.489  < 2e-16 ***
## drg122:age             -1.712e-03  7.607e-04  -2.251 0.024396 *  
## drg123:age             -3.473e-03  1.335e-03  -2.602 0.009275 ** 
## drg122:charges.na      -1.566e-01  4.248e-02  -3.686 0.000229 ***
## drg123:charges.na      -6.514e-02  6.815e-02  -0.956 0.339178    
## charges:age            -2.009e-07  5.760e-08  -3.488 0.000488 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for Gamma family taken to be 0.2609842)
## 
##     Null deviance: 6557.8  on 12843  degrees of freedom
## Residual deviance: 3270.1  on 12796  degrees of freedom
## AIC: 64956
## 
## Number of Fisher Scoring iterations: 7
```

As diagnostics to evaluate the fit of our model, we produced a plot of residual deviance and Cook's distances.


```r
par(mfrow=c(1,2))
# Residuals plot
plot(residuals(gamma_model)~fitted(gamma_model),
     xlab="Fitted Values",
     ylab="Residual Values", 
     main="Residual vs. Fitted Values Plot")
abline(h = -2, lty = 2, col = 'red')
abline(h = 2, lty = 2, col = 'red')
# Cook's distance plot
plot(cooks.distance(gamma_model), type="h", lwd=2,
  xlab="Observation index",
  ylab="Cook's distances",
  main="Cook's Distances Plot")
abline(h=1,lty=2,col="red")
```

![](gamma_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


The residuals plot indicates there are a handful of outliers that breach the +/- 2 threshold, but the Cook's distance plot shows there are no influential points of note. 

Finally, we examined the potential for collinearity using the Variance Inflation Factor (VIF).


```r
library(car)
vif(gamma_model)
```

```
##                           GVIF Df GVIF^(1/(2*Df))
## diagnosis         2.292418e+06  8        2.497574
## sex               3.036608e+01  1        5.510542
## drg               3.653242e+03  2        7.774449
## charges           5.383645e+01  1        7.337333
## age               8.542817e+00  1        2.922810
## charges.na        3.518046e+00  1        1.875645
## diagnosis:drg     7.228644e+05 16        1.524388
## diagnosis:charges 2.723678e+05  8        2.186237
## sex:age           2.773219e+01  1        5.266136
## sex:charges.na    2.619160e+00  1        1.618382
## drg:charges       1.149419e+01  2        1.841279
## drg:age           1.218950e+03  2        5.908762
## drg:charges.na    2.452613e+00  2        1.251432
## charges:age       3.581126e+01  1        5.984251
```

There are a number of terms with a GVIF^(1/(2*Df)) greater than $\sqrt{10}$, though we suspect this is due to the presence of interaction terms. We therefore fit the baseline model without interaction terms and redid the VIF inspection.



```r
baseline_model = glm(formula = los ~ diagnosis + sex + drg + charges + 
    age + charges.na, family = Gamma(log), data = ami)
vif(baseline_model)
```

```
##                GVIF Df GVIF^(1/(2*Df))
## diagnosis  1.041196  8        1.002526
## sex        1.103326  1        1.050393
## drg        1.197861  2        1.046169
## charges    1.059859  1        1.029494
## age        1.223870  1        1.106287
## charges.na 1.001257  1        1.000629
```

These values are all much less than $\sqrt{10}$, suggesting there is no multicollinearity in the base predictors, and the multicollinearity present in the full model comes from the interaction terms.


