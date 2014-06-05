mydf <- data.frame(x=c( 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27),
                   y=c(4,2.5,7,7.5,5,4.5,9,8.5,7,10.5,8,11.5,6,10.5,10,7.5,9,11.5,15,14.5,15,14.5,15,13.5,16,16.5,16,17.5))



x=c( 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)
y=c(4,2.5,7,7.5,5,4.5,9,8.5,7,10.5,8,11.5,6,10.5,10,7.5,9,11.5,15,14.5,15,14.5,15,13.5,16,16.5,16,17.5)
> cor(x,y)
[1] 0.9133641
> fit<-lm(y~x)
> fit

Call:
  lm(formula = y ~ x)

Coefficients:
  (Intercept)            x  
4.0172       0.4776  

> plot(fit)

> attributes(fit)
$names
[1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values" "assign"       
[7] "qr"            "df.residual"   "xlevels"       "call"          "terms"         "model"        

$class
[1] "lm"


> summary(fit)

Call:
  lm(formula = y ~ x)

Residuals:
  Min      1Q  Median      3Q     Max 
-3.7479 -0.8602  0.3643  1.4148  2.3867 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.01724    0.65680   6.116 1.82e-06 ***
  x            0.47756    0.04175  11.439 1.20e-11 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.784 on 26 degrees of freedom
Multiple R-squared:  0.8342,  Adjusted R-squared:  0.8279 
F-statistic: 130.8 on 1 and 26 DF,  p-value: 1.202e-11

> cc=coef(fit)
> cc
(Intercept)           x 
4.0172414   0.4775588 

xnew<-c(12.5, 13.5,14.5)
> ynew=cc[1]+cc[2]*xnew
> ynew
[1]  9.986727 10.464286 10.941845


m3=lm(formula = PBE ~ CBE + PBO + CPO + PFO +DINC + CFO+RDINC+RFP+YEAR, data = beef)