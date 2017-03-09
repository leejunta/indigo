require(ggplot2)
require(mosaic)
setwd("~/Documents/Projects/Website/juntaeklee/assets/R/")

#Sigmoid function
x <- c(-100:100)/10
y <- 1.0/(1+exp(-x))
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
X <- data.frame("x0" = rep(1,200), "x1" = rnorm(200,10,1))
y <- sapply(x$x1, function(x) rnorm(1,x,5))
sigmoid <- function(x) {
    1.0/(1+exp(-x))
}
lincostf <- function(X,y,theta) {
    m <- length(y)
    1/(2*m)*sum(((as.matrix(X) %*% as.matrix(theta))-y)^2)
}

costs <- sapply(c(-100:100), function(x) lincostf(X,y,c(0,x)))
index <- c(1:length(costs))
dflincosts <- data.frame(costs,index)

#week4lincostplot
ggplot(data = dflincosts, aes(x=index, y=costs)) + 
    geom_point()  + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5,
                                    face = 'bold',
                                    size = 16)) + 
    labs(title="Linear Regression Cost Function",
         x = "Theta",
         y = "Cost Function")
