library(ggplot2)
library(spatstat)
n <- 100

random_noise <- rnorm(n, mean = 0, sd = 0.1)
random_outlier <-c(random_noise,50)
fd <- data.frame(Y <- random_outlier,X =c(1:100,150))

ggplot(data = fd,aes(x=X,y=Y)) + geom_point()
cor(1:n,random_noise)

cor(1:(n+1),random_outlier)


# radius of the circle
plot(runifdisc(10))



r <- sqrt(runif(100))
t <- 2*pi*runif(100)
x <- r*cos(t) ; y <- r*sin(t)
dat <- data.frame(x=x,y=y)

xnew <- c(x,10);ynew <- c(y,10)
dat_new <- data.frame(xnew,ynew)

plot(x,y,pch=20)
plot(xnew,ynew,pch=20,xlab="X",ylab="Y",main="Correlation for model with and without outlier")
cor(x,y)
cor(xnew,ynew)
lm1 <- lm(y ~x,data =dat)
lm2 <- lm(ynew ~xnew,data =dat_new)
abline(a=0.0048,b=0.008,col="red",lwd=2)
abline(a=-0.02,b=0.78,lwd=2)

text(9.3,9.7,label = "Outlier")
text(4,5,label = "Correlation 0.79",cex = 1.2)
text(6,0.6,label = "Correlation 0.0087",cex = 1.2,col="red")

legend("topleft",c("Model without outlier","Model with outlier"),col=c("red","black"),lty=c(1,1))
