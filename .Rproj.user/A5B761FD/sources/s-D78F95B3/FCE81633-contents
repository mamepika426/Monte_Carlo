#List1.1
#diamonds dataset
View(diamonds)
data(diamonds)

#分割表
table(diamonds[,c(2,3)])
==================================================================
#List1.2
c <- matrix(c(rep(0:1,each=3),rep(0:1,3)),ncol=2)
c
(card <- rmultinom(1,10000,prob=rep(1/6,6)))
(r <- sum(card*c[,1]))
rr <- sum(card*c[,1]*c[,2])
rr
rr/r

#List1.3
library("ggplot2")
data <- c(3,2,2,4,4,6,4,5)
ggplot(data.frame(x=c(2,5)),aes(x))+stat_function(fun=dgamma,args=list(shape=1+sum(data),rate=1+length(data)))+theme_bw()


#List1.5
if(0){  #うまくいかない?
data("ToothGrowth")
str(ToothGrowth)
View(ToothGrowth)
x<-ToothGrowth$len
y<-ToothGrowth$supp
sd<-sd(x)
n1<-sum(y=="OJ")
x1<-x[y=="OJ"]
mu1<-sum(x1)/(1+n1)
n2<-sum(y=="VC")
x2<-x[y=="VC"]
mu2<-sum(x2)/(1+n2)
q_denominator<-1+sqrt((1+n1+n2)/(1+n1)*(1+n2))*exp((-1/(2*sd^2))*(  ((sum(x))^2)/(1+n1+n2) - ((sum(x1))^2)/(1+n1) - ((sum(x2))^2)/(1+n2) ))
q<-1/q_denominator
q
}                   


data("ToothGrowth")
str(ToothGrowth)
View(ToothGrowth)
x<-ToothGrowth$len
y<-ToothGrowth$supp
sd<-sd(x)
n1<-sum(y=="OJ")
x1<-x[y=="OJ"]
mu1<-sum(x1)/(1+n1)
sd1<-sd*sqrt(1+sum(y=="OJ"))
n2<-sum(y=="VC")
x2<-x[y=="VC"]
mu2<-sum(x2)/(1+n2)
sd2<-sd*sqrt(1+sum(y=="VC"))
q<-1/(1+sqrt(1+n1+n2)/((1+n1)*(1+n2))*exp(-(sum(x^2)/(1+n1+n2)-sum(x1^2)/(1+n1)-sum(x2^2)/(1+n2))/(2*sd^2)))
log10(q/(1-q))

#List1.6
data("airquality")
str(airquality)
x<-airquality$Solar.R[is.na(airquality$Solar.R)+is.na(airquality$Ozone)==0]
y<-airquality$Ozone[is.na(airquality$Solar.R)+is.na(airquality$Ozone)==0]  #x,yに欠損していないSolar.RとOzone入れる
sd<-31.33
ggplot(airquality,aes(x=Ozone))+stat_function(fun=dnorm,args=list(mean=sum(y)/(1+length(y)),sd=sd/sqrt(1+length(y))),color="blue")+
  theme_bw()+xlim(35,50)

ggplot(airquality,aes(x=Ozone))+stat_function(fun=dnorm,args=list(mean=sum(x*y)/(1+sum(x^2)),sd=sd/sqrt(1+sum(x^2))),color="blue")+
  theme_bw()+xlim(0.16,0.28)

log10(exp(1/(2*sd^2)*sum(x*y)^2/(1+sum(x^2)))*1/sqrt(1+sum(x^2)))


install.packages("plotly")
library(plotly)
library(magrittr)
plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species,type="scatter")

is.data.frame(iris)

###練習問題1.9(ディリクレ分布)
N <- 1e3
alpha <- c(1,1.5,1.8)
x <- rgamma(N,alpha[1])
y <- rgamma(N,alpha[2])
z <- rgamma(N,alpha[3])
w <- x+y+z
plot_ly(x=x/w,y=y/w,z=z/w,type="scatter3d",mode="markers",size=0.1)










