rm(list=ls())
if (!require("lattice")) {
  install.packages("lattice")
  require("lattice")
}

str(iris)
histogram(~Petal.Length, data=iris, type="count")
histogram(~Petal.Length | Species, data=iris, type="count")
histogram(~Petal.Length | Species, data=iris, type="count", layout=c(1,3))
histogram(~Petal.Length | Species, data=iris, type="count", layout=c(1,3),
          strip=FALSE, strip.left=TRUE)

densityplot(~Petal.Length, data=iris)
densityplot(~Petal.Length|Species, data=iris)
densityplot(~Petal.Length|Species, data=iris, layout=c(1,3))
densityplot(~Petal.Length|Species, data=iris, layout=c(1,3), strip=FALSE, strip.left=TRUE)
densityplot(~Petal.Length, groups=Species, data=iris)
densityplot(~Petal.Length, groups=Species, data=iris, auto.key=TRUE)

# scatterplots
xyplot(Sepal.Length~Petal.Length, data=iris)
xyplot(Sepal.Length+Sepal.Width~Petal.Length, data=iris, auto.key=TRUE)
xyplot(Sepal.Length+Sepal.Width~Petal.Length+Petal.Width,            data=iris, auto.key=TRUE)
xyplot(Sepal.Length+Sepal.Width~Petal.Length+Petal.Width | Species,  data=iris, auto.key=TRUE)

m = mtcars
m$am = factor(mtcars$am, labels=c("auto","man"))
str(m)
summary(m)
print(xyplot(hp~disp, data=m)) # scatterplot; try "source()"
xyplot(hp~disp, data=m, type=c("p","r","g","smooth"))
xyplot(hp~disp | am, data=m) # "|" => separate panels
xyplot(hp~disp, groups=am, data=m, auto.key=TRUE) # "groups" => different colors
xyplot(hp~disp, groups=am, data=m, type=c("p","r"), auto.key=TRUE)
xyplot(hp~disp, groups=am, data=m, auto.key=TRUE,
       main="mtcars", xlab="Displacement", ylab="Horsepower") # add labels

x = seq(from=-3, to=3, length.out=10) # plot N(0,1)
xyplot(dnorm(x) ~ x)
xyplot(dnorm(x) ~ x, type="l")
# "+" indicates plotting both "dnorm(x) ~ x" and "dnorm(x, mean=1, sd=2) ~ x"
xyplot(dnorm(x) + dnorm(x, mean=1, sd=2) ~ x, type="l")
?xyplot # how to add key?
?simpleKey
xyplot(dnorm(x) + dnorm(x, mean=1, sd=2) ~ x, type="l", auto.key=TRUE)
xyplot(dnorm(x) + dnorm(x, mean=1, sd=2) ~ x, type="l",
       auto.key=list(points=FALSE, lines=TRUE, text=c("N(0,1)","N(1,2)")))

barchart(table(mtcars$cyl,mtcars$gear),auto.key=list(title="gear"),ylab="cyl")
barchart(Titanic, auto.key=TRUE)
barchart(Titanic, auto.key=list(title="Survived"))
barchart(Titanic, scales=list(x="free"), auto.key=list(title="Survived"))

str(singer)
summary(singer)
bwplot(~ height, data=singer, xlab="Height (inches)")
bwplot(voice.part ~ height, data=singer, xlab="Height (inches)")

# 1-D scatterplot, especially for small samples
stripplot(~ jitter(height), data=singer, jitter.data=TRUE, xlab="Height (in)")
stripplot(voice.part ~ jitter(height), data=singer, jitter.data=TRUE, xlab="Height (in)")

# 3-D plots
str(quakes)
cloud(depth ~ lat * long, data = quakes) # 3-D scatterplot

str(volcano)         # a matrix
wireframe(volcano)   # 3-D surface (matrix interface)
contourplot(volcano) # (matrix interface)

qmap(baylor, zoom = 14, source = "stamen", maptype = "watercolor")