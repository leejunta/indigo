require(ggplot2)
require(mosaic)
setwd("~/Documents/Projects/Website/juntaeklee/assets/R/")

#Sigmoid function
X <- c(-100:100)/10
y <- 1.0/(1+exp(-X))
dfsigmoid <- data.frame(x,y)
ggplot(data = dfsigmoid, aes(x=x, y=y)) + 
    geom_line()  + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5,
                                    face = 'bold',
                                    size = 16)) + 
    labs(title="Sigmoid Function",
         x = "x",
         y = "sig(x)") 

#using linear cost function for logistic regression
X <- data.frame("x0" = rep(1,200), "x1" = rnorm(200,0,1), "x1^2" = rnorm(200,0,1)^2)
y <- sapply(X$x1, function(x) rnorm(1,x,1))
sigmoid <- function(x) {
    1.0/(1+exp(-x))
}
lincostf <- function(X,y,theta) {
    m <- length(y)
    1/(2*m)*sum(((as.matrix(X) %*% as.matrix(theta))-y)^2)
}

costs <- sapply(c(-100:100), function(x) lincostf(X,y,c(0,x,x)))
index <- c(-100:100)
dflincosts <- data.frame(costs,index)

#week4lincostplot
ggplot(data = dflincosts, aes(x=index, y=costs)) + 
    geom_line()  + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5,
                                    face = 'bold',
                                    size = 16)) + 
    labs(title="Linear Regression Cost Function",
         x = "Theta",
         y = "Cost Function")


lincostfs <- function(X,y,theta) {
    m <- length(y)
    1/(2*m)*sum((sigmoid((as.matrix(X) %*% as.matrix(theta)))-y)^2)
}
ysig <- sigmoid(y)
ysig[ysig >= 0.5] <- 1
ysig[ysig < 0.5] <- 0
ysig <- sample(x = c(0,1),200,replace=T)
Xsig <- data.frame("x0" = X$x0,"x1" = X$x1,"x2" = X$x1^2, "x3" = X$x1^3)
costs <- sapply(c(-100:100), function(x) lincostfs(Xsig,ysig,c(0,x,1,1)))
index <- c(-100:100)
dfsigcosts <- data.frame(costs,index)

#week4lincostplot
ggplot(data = dfsigcosts, aes(x=index, y=costs)) + 
    geom_line()  + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5,
                                    face = 'bold',
                                    size = 16)) + 
    labs(title="Linear Regression Cost Function with Sigmoid",
         x = "Theta",
         y = "Cost Function")
