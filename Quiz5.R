## Consider testing H_0: mu = 9.4 against H_1: mu > 9.4,
#given this random sample from a normal population: 
#11.6, 11.2, 9.6, 10.7, 11.2, 8, 11.8, 13.5, 9.3, 9.6, 6.5, 10.5, 8.8, 10.1, 11, 11.4, 10.8, 7.9, 11.6, 10.6, 9.9. Find the value of the test statistic, t.

samp <-c(3, 6, 7.1, 3.8, 5.7, 7.1, 5.6, 9.6, 5.2, 7.4, 6.7, 8.4, 6.7, 7.7, 9.5, 5.4, 6.9, 6.7, 4.8, 10.8)
t.test(samp,alternative="greater",mu=6.8)
-----------------------------------------
  samp <-c(3, 6, 7.1, 3.8, 5.7, 7.1, 5.6, 9.6, 5.2, 7.4, 6.7, 8.4, 6.7, 7.7, 9.5, 5.4, 6.9, 6.7, 4.8, 10.8)

> t.test(samp, alternative = "two.sided", conf.level = 0.9)
One Sample t-test

data:  samp
t = 15.6055, df = 19, p-value = 2.738e-12
alternative hypothesis: true mean is not equal to 0
90 percent confidence interval:
  5.962068 7.447932
sample estimates:
  mean of x 
6.705 
--------------------------------------------------

  18 18 12
10 11 15
18 16 10

> two_way<-matrix(c(18,18,12,10,11,15,18,16,10),ncol=3,byrow=TRUE)
> two_way
[,1] [,2] [,3]
[1,]   18   18   12
[2,]   10   11   15
[3,]   18   16   10

> chisq.test(two_way)

Pearson's Chi-squared test

data:  two_way
X-squared = 4.1746, df = 4, p-value = 0.3829

--------------------------------------------------------------------
> prop.test(38,n=56,p=1/2,correct=FALSE)

  1-sample proportions test without continuity correction

data:  38 out of 56, null probability 1/2
X-squared = 7.1429, df = 1, p-value = 0.007526
alternative hypothesis: true p is not equal to 0.5
95 percent confidence interval:
 0.5482264 0.7859901
sample estimates:
        p 
0.6785714 
---------------------------------------------------------------
> success=dbinom(38,56,0.5)
> success
[1] 0.002946643
---------------------------------------------------------------
> ?t.test
> var.test(X,Y)

  F test to compare two variances

data:  X and Y
F = 0.9791, num df = 8, denom df = 4, p-value = 0.9033
alternative hypothesis: true ratio of variances is not equal to 1
95 percent confidence interval:
 0.1090342 4.9469368
sample estimates:
ratio of variances 
         0.9790811 
---------------------------------------------------------------
