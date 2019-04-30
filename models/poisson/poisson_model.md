---
title: "Poisson"
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





# Poisson Regression Model

The reponse variable, Length of Stay (LOS), has a minimum value of 0 and a maximum value of 38. Our first idea is to investigate the poisson model; we did not invetigate the zero-flated model because the current dataset only had 1 observation with a 0 length of stay. It makes sense to model LOS as a Poisson count process because we can treat individual patient as the "confined space," and count how many days (0 or larger) the patient stayed in the hospital since admittance. To select the best-performing model, we used stepwise selection. 


```r
#stepwise model selection for poisson regression model
best_model <- step(glm(los ~ ., family = 'poisson', data = ami), trace=0, direction='both')
summary(best_model)
```

```
## 
## Call:
## glm(formula = los ~ diagnosis + sex + drg + charges + age + charges.na, 
##     family = "poisson", data = ami)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -5.4912  -0.9315  -0.0939   0.6206   9.1299  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     9.610e-01  2.644e-02  36.341  < 2e-16 ***
## diagnosis41011 -1.062e-02  1.862e-02  -0.570   0.5684    
## diagnosis41021 -3.443e-03  2.838e-02  -0.121   0.9035    
## diagnosis41031 -6.294e-02  2.847e-02  -2.211   0.0270 *  
## diagnosis41041 -1.711e-02  1.811e-02  -0.945   0.3448    
## diagnosis41051  5.969e-02  3.417e-02   1.747   0.0807 .  
## diagnosis41071 -1.978e-02  1.879e-02  -1.053   0.2925    
## diagnosis41081 -5.513e-02  2.776e-02  -1.986   0.0471 *  
## diagnosis41091  3.094e-02  1.737e-02   1.781   0.0749 .  
## sexM           -4.834e-02  6.792e-03  -7.117  1.1e-12 ***
## drg122         -6.282e-02  7.067e-03  -8.890  < 2e-16 ***
## drg123         -6.208e-01  1.335e-02 -46.495  < 2e-16 ***
## charges         5.200e-05  3.816e-07 136.273  < 2e-16 ***
## age             8.597e-03  2.648e-04  32.467  < 2e-16 ***
## charges.na      1.321e-01  1.380e-02   9.570  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 41065  on 12843  degrees of freedom
## Residual deviance: 19630  on 12829  degrees of freedom
## AIC: 66502
## 
## Number of Fisher Scoring iterations: 5
```

The result above suggested that the full model best predicts the length of stay. The summary suggests that there is potentially a lack of fit by the model as the dispersion parameter approximates $\frac{19630}{12829} \approx 1.52$, rather than $\phi = 1$, which is assumed by the Poisson regression model. The diagnostic plots below do not suggest any significant influential observations. However, the residual vs. fitted plot suggests our model lack fit because the residuals display a fan-like pattern with values beyond the acceptable -2 and 2 bound.


```r
par(mfrow=c(1,1))
#rvf
plot(residuals(best_model)~fitted(best_model),
     xlab="Fitted Values",
     ylab="Residual Values", 
     main="RVF Plot for the Poisson Model")
abline(h=-2,lty=2,col="red")
abline(h=2,lty=2,col="red")
```

![](poisson_model_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#plot cook's distance
plot(cooks.distance(best_model), type="h", lwd=2,
  xlab="Observation index",
  ylab="Cook's distances",
  main="Cook's distances for the Poisson Model")
abline(h=1,lty=2,col="red")
```

![](poisson_model_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

The grouped variance inflation factors do not suggest any issue with multicollinearity.


```r
#check vif
car::vif(best_model)
```

```
##                GVIF Df GVIF^(1/(2*Df))
## diagnosis  1.041692  8        1.002556
## sex        1.098708  1        1.048193
## drg        1.164420  2        1.038789
## charges    1.084860  1        1.041566
## age        1.197368  1        1.094243
## charges.na 1.008948  1        1.004464
```


# Adding Interaction Terms

In this section we explore the effect of introducing interaction terms to our model. The motivation behind this is that some of our predictor variables could be interacting with each other to affect patients' length of stay. Because our model includes primarily categorical predictors, we only explored 2-way interactions. 


```r
# With interaction terms
best_int <- step(glm(los ~ .^2, family = 'poisson', data = ami), trace=0, direction='both')
summary(best_int)
```

```
## 
## Call:
## glm(formula = los ~ diagnosis + sex + drg + charges + age + charges.na + 
##     diagnosis:drg + diagnosis:charges + diagnosis:charges.na + 
##     sex:age + sex:charges.na + drg:charges + drg:age + drg:charges.na + 
##     charges:age, family = "poisson", data = ami)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -6.7842  -0.9093  -0.1057   0.6131   8.8473  
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                7.240e-01  6.073e-02  11.921  < 2e-16 ***
## diagnosis41011             1.150e-01  4.300e-02   2.676 0.007459 ** 
## diagnosis41021             2.567e-02  6.836e-02   0.375 0.707329    
## diagnosis41031            -2.552e-01  7.257e-02  -3.517 0.000437 ***
## diagnosis41041            -3.284e-03  4.230e-02  -0.078 0.938124    
## diagnosis41051            -9.232e-02  8.288e-02  -1.114 0.265328    
## diagnosis41071             1.777e-02  4.319e-02   0.411 0.680723    
## diagnosis41081            -1.012e-01  6.556e-02  -1.543 0.122790    
## diagnosis41091             4.207e-02  4.031e-02   1.044 0.296638    
## sexM                       1.273e-01  3.743e-02   3.400 0.000675 ***
## drg122                    -7.735e-02  5.271e-02  -1.467 0.142257    
## drg123                    -7.689e-01  1.062e-01  -7.243 4.39e-13 ***
## charges                    5.585e-05  2.835e-06  19.698  < 2e-16 ***
## age                        1.272e-02  6.505e-04  19.554  < 2e-16 ***
## charges.na                 3.374e-01  6.542e-02   5.158 2.50e-07 ***
## diagnosis41011:drg122     -7.598e-02  4.107e-02  -1.850 0.064335 .  
## diagnosis41021:drg122      2.134e-02  6.181e-02   0.345 0.729944    
## diagnosis41031:drg122      1.052e-01  6.149e-02   1.710 0.087244 .  
## diagnosis41041:drg122      1.053e-02  3.977e-02   0.265 0.791157    
## diagnosis41051:drg122      7.848e-02  7.415e-02   1.058 0.289902    
## diagnosis41071:drg122     -2.292e-02  4.135e-02  -0.554 0.579425    
## diagnosis41081:drg122     -6.671e-02  6.150e-02  -1.085 0.278062    
## diagnosis41091:drg122      4.918e-03  3.839e-02   0.128 0.898061    
## diagnosis41011:drg123     -1.942e-01  6.476e-02  -2.998 0.002714 ** 
## diagnosis41021:drg123     -2.631e-01  1.051e-01  -2.504 0.012294 *  
## diagnosis41031:drg123     -2.041e-01  1.165e-01  -1.751 0.079865 .  
## diagnosis41041:drg123     -4.199e-02  6.637e-02  -0.633 0.527001    
## diagnosis41051:drg123      2.762e-01  1.114e-01   2.481 0.013119 *  
## diagnosis41071:drg123     -9.333e-02  7.251e-02  -1.287 0.198037    
## diagnosis41081:drg123      5.092e-02  9.460e-02   0.538 0.590371    
## diagnosis41091:drg123     -5.712e-02  5.981e-02  -0.955 0.339536    
## diagnosis41011:charges    -5.043e-06  2.071e-06  -2.435 0.014898 *  
## diagnosis41021:charges    -2.266e-06  3.464e-06  -0.654 0.513070    
## diagnosis41031:charges     1.235e-05  4.054e-06   3.046 0.002323 ** 
## diagnosis41041:charges     1.609e-07  2.063e-06   0.078 0.937830    
## diagnosis41051:charges     8.109e-06  4.998e-06   1.622 0.104733    
## diagnosis41071:charges    -5.444e-07  2.089e-06  -0.261 0.794371    
## diagnosis41081:charges     6.335e-06  3.192e-06   1.985 0.047171 *  
## diagnosis41091:charges     2.754e-07  1.957e-06   0.141 0.888074    
## diagnosis41011:charges.na -5.538e-02  7.491e-02  -0.739 0.459753    
## diagnosis41021:charges.na  3.070e-02  1.066e-01   0.288 0.773404    
## diagnosis41031:charges.na  7.983e-02  1.266e-01   0.631 0.528234    
## diagnosis41041:charges.na -1.817e-01  7.288e-02  -2.493 0.012656 *  
## diagnosis41051:charges.na  6.745e-02  1.573e-01   0.429 0.667993    
## diagnosis41071:charges.na -1.064e-01  7.563e-02  -1.407 0.159558    
## diagnosis41081:charges.na -5.007e-02  1.165e-01  -0.430 0.667311    
## diagnosis41091:charges.na -1.193e-01  6.786e-02  -1.758 0.078670 .  
## sexM:age                  -2.470e-03  5.292e-04  -4.668 3.05e-06 ***
## sexM:charges.na           -9.572e-02  2.825e-02  -3.389 0.000703 ***
## drg122:charges             1.178e-05  9.252e-07  12.730  < 2e-16 ***
## drg123:charges             2.287e-05  1.132e-06  20.194  < 2e-16 ***
## drg122:age                -1.752e-03  5.391e-04  -3.249 0.001157 ** 
## drg123:age                -1.370e-03  1.195e-03  -1.146 0.251784    
## drg122:charges.na         -1.266e-01  2.944e-02  -4.301 1.70e-05 ***
## drg123:charges.na         -7.439e-02  5.899e-02  -1.261 0.207262    
## charges:age               -1.337e-07  3.082e-08  -4.338 1.44e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 41065  on 12843  degrees of freedom
## Residual deviance: 18945  on 12788  degrees of freedom
## AIC: 65900
## 
## Number of Fisher Scoring iterations: 5
```

The best model with interaction suggested by the stepwise selection process includes all interactions, except those between sex and drug, and sex and diagnosis. However, the dispersion factor approximation also appears to be off the chart as $\hat{\phi} \approx \frac{18945}{12788} \approx 1.48$. The diagnostic below renders the same conclusions as above.


```r
par(mfrow=c(1,1))
#rvf
plot(residuals(best_int)~fitted(best_int),
     xlab="Fitted Values",
     ylab="Residual Values", 
     main="RVF Plot for the Poisson Model with Interactions")
abline(h=-2,lty=2,col="red")
abline(h=2,lty=2,col="red")
```

![](poisson_model_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#plot cook's distance
plot(cooks.distance(best_int), type="h", lwd=2,
  xlab="Observation index",
  ylab="Cook's distances",
  main="Cook's distances for the Poisson Model with Interactions")
abline(h=1,lty=2,col="red")
```

![](poisson_model_files/figure-html/unnamed-chunk-6-2.png)<!-- -->



