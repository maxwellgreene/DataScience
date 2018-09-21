hello_world <- function(name)
{
  print(sprintf("Hello, %s",name))
}

complex_computation <- function(x,a=1,b=0,c=0)
{
  y <- a*x^2+b*x+c
  return(y)
}

x <- seq(-2,3,by = 0.1)
y <- complex_computation(x,a=-1,b=2,c=-3)
library(ggplot2)
ggplot(data=NULL,mapping=aes(x=x,y=y))+geom_line()

my_bmi <- function(mass,height)
{
  return(mass/height^2)
}