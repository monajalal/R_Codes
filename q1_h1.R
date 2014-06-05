cigarettes <- read.table("http://pages.stat.wisc.edu/~gvludwig/327-5/cigarettes.dat", 
                         row.names = 1)
names(cigarettes) <- c("Tar", "Nicotine", "Weight", "CO")
cigarettes
#a. Write R code that calculates the correlations between CO and each of the columns Tar, Nicotine and Weight, and show them.
cor(cigarettes$CO, cigarettes$Tar)
cor(cigarettes$CO, cigarettes$Nicotine)
cor(cigarettes$CO, cigarettes$Weight)
#Write R code that fits a linear model of CO explained by Tar, Nicotine and Weight.
fit.model=lm(formula = CO ~ ., data = cigarettes)
summary(fit.model)
plot(fit.model, which=1:2)
#the same as lm(formula = CO ~ Tar + Nicotine + Weight, data = cigarettes)
#Do the coefficients estimated correspond to your expectations?
X=model.matrix(fit.model)
str(X)
result=t(X) %*% X
mat.solve=solve(t(X)%*%X)
(XbyX <- crossprod(X))
eigen(XbyX)
eigen(XbyX, symmetric=TRUE, only.values=TRUE)
#c. Fit a model that explains CO by Tar and Weight. How did the model change? Does your interpretation of the coefficients remain the same?
fit.model2=lm(formula = CO ~ Tar+Weight, data = cigarettes)
coeffs2=fit.model2$coefficients
#d. Fit a model that explains CO by Nicotine and Weight. How did the model change? Does your interpretation of the coefficients remain the same?
fit.model3=lm(formula = CO ~ Nicotine+Weight, data = cigarettes)
coeffs3=fit.model3$coefficients