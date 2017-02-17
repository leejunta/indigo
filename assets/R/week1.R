#week1graph1
x <- c(1:5)
y <- c(2,2,3,3.5,4.5)
dff <- data.frame(x,y)

ggplot(data = dff, aes(x=x, y=y)) + 
    geom_point()  + 
    theme(legend.position="none") + 
    labs(title="") + 
    scale_x_continuous(limits = c(1,5)) + 
    scale_y_continuous(limits = c(0,5)) +
    geom_hline(yintercept = 3.5)

#week1graph2
ggplot(data = dff, aes(x=x, y=y)) + 
    geom_point()  + 
    theme(legend.position="none") + 
    labs(title="") + 
    scale_x_continuous(limits = c(1,5)) + 
    scale_y_continuous(limits = c(0,5)) +
    geom_smooth(method='lm',formula=y~x, se=F)

#parabola
x <- 1:5
dat <- data.frame(x,y=(x-3)^2+1)
f <- function(x) (x-3)^2+1
ggplot(dat, aes(x,y)) + 
    geom_point()+
    stat_function(fun=f, colour="red") +
    scale_y_continuous(limit = c(0,5)) 

