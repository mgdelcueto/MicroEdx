#Preliminaries
rm(list=ls())
library("utils")
#install.packages('plot3D')
library(plot3D)

#Creating the vectors X and Y
M<-mesh(seq(0,1,length=100),seq(0,1,length=100))
x<-M$x
y<-M$y
z<-6/5*(x+y^2)

persp3D(x,y,z,xlab='X variable',ylab='Y variable',xlim=c(0,1),main='Plotting joint PDF')

x<-seq(0,1,length=100)
Fx<-6/5*(1/2*x^2+1/3*x)
Fy<-6/5*(1/2*x^2+1/2*x)
plot(x,Fx)
plot(x,Fy)