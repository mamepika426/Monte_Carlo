#List3.1
f <- function(x){
  return(1/cos(x)+1/(1+x^2))
}
N <-100
t <- seq(-1,1,length=N)
sum(as.numeric(lapply(t[-1],f))+as.numeric(lapply(t[-N],f)))*2/(2*(N-1))

t_3 <- seq(-1,1,length=2^3)
g_3 <- as.numeric(lapply(t_3,f))
df_3 <- data.frame(x=t_3,y=g_3)
t_4 <- seq(-1,1,length=2^4)
g_4 <- as.numeric(lapply(t_4,f))
df_4 <- data.frame(x=t_4,y=g_4)

##これがんばったやつ
ggplot(data.frame(x=seq(-1,1,by=0.01)),aes(x))+
  stat_function(fun=f)+
  annotate("text",x=0,y=2.3,
           label=paste("italic(y)==frac(1,cos(x))+frac(1,1+x^2)"),parse=TRUE,size=6)+
  geom_point(data=df_3,aes(x=t_3,y=g_3),color="blue")+
  geom_line(data=df_3,aes(x=t_3,y=g_3),color="blue",linetype="dashed")+
  geom_point(data=df_4,aes(x=t_4,y=g_4),color="red")+
  geom_line(data=df_4,aes(x=t_4,y=g_4),color="red",linetype="dotdash")
#parse=TRUEで数式を解釈して表示

#List3.2
set.seed(1234)
N <- 1e1
M <- 1e6
x <- rcauchy(N)+10
f <- function(theta){
  return(prod(dcauchy(x-theta))*dcauchy(theta))
}
M <- 10000
L <- -100
U <- 100
t <- seq(U,L,length=M)
sum(as.numeric(lapply(t[-1],f))+as.numeric(lapply(t[-M],f)))*(U-L)/(2*M)

#List 3.3
f <- function(x){
  return((cos(50*x)+sin(20*x))^2)
}
#関数の表示
ggplot(data.frame(x=seq(0,1,by=0.001)),aes(x))+
  stat_function(fun=f)

#収束の様子を確認
true_value <- integrate(f,lower=0,upper=1)
true_y <- rep(as.numeric(true_value[1]),201)
true <- data.frame(x=seq(0,3e+05,by=1000),y=true_y)

num_data <- seq(0,3e+05,by=1000)

monte <- numeric(301)
for(i in 1:301){
  monte[i] <- mean(f(runif(num_data[i])))
}
data3_3 <- data.frame(x=num_data,y=monte)

ggplot(data.frame(x=seq(0,3e+05,by=1000)),aes(x))+
  geom_line(data=data3_3,aes(x=num_data,y=monte),color="blue")+
  geom_line(data=true,aes(x=x,y=true_y),linetype="dashed",color="red",size=1)+
  ylim(0.92,1)
  

#List3.4 
set.seed(1234)
N <- 1e1
M <- 1e6
x <- rcauchy(N)
f <- function(theta){
  return(prod(dcauchy(x-theta)))
}
theta <- dcauchy(M)
mean(as.numeric(lapply(theta,f)))

#List3.5
f <- function(x){
  if(x>2){1}
  else {0}
}

x <- 1:1000
y <- numeric(0)
z <- numeric(0)
u <- rcauchy(max(x))

for(N in x){
  t <- u[1:N]
  y <- append(y,sum(as.numeric(lapply(t,f)))/N)
  z <- append(z,sum((as.numeric(lapply(t,f))-sum(as.numeric(lapply(t,f)))/N)^2)/(N-1)/N)
}

data.fr <- data.frame(x=x,y=y,z=z)

ggplot(data.fr,aes(x=x,y=y))+geom_line(size=2)+geom_ribbon(aes(ymin=y-1.96*sqrt(z),ymax=y+1.96*sqrt(z)),alpha=0.3)+
  geom_hline(yintercept=0.1476,lty=2)+theme_bw()+ylim(-0.1,0.5)
  
#List 3.6
set.seed(1234)
#真の値
theta0 <- 1.5
x <- rnorm(theta0,1)

f <- function(theta){
  theta/(1+theta^2)
}

g <- function(theta){
  1/(1+theta^2)
}

M <- 1000

y <- numeric(0)
z <- numeric(0)
u <- rnorm(M)+x

for(N in 1:M){
  t <- u[1:N]
  fseq <- as.numeric(lapply(t,f))
  gseq <- as.numeric(lapply(t,g))
  fmean <- mean(fseq)
  gmean <- mean(gseq)
  fvar <- sd(fseq)^2
  gvar <- sd(gseq)^2
  fgcov <- cov(fseq,gseq)
  y <- append(y,fmean/gmean)
  z <- append(z,tail(y,n=1)^2*(fvar/fmean^2-2*fgcov/(fmean*gmean)+gvar/gmean^2)/N)
}

data.fr <- data.frame(x=1:M,y=y,z=z)

ggplot(data.fr,aes(x=x,y=y))+geom_line(color="blue",size=2)+
  geom_ribbon(aes(ymin=y-1.96*sqrt(z),ymax=y+1.96*sqrt(z)),alpha=0.3,fill="blue")+theme_bw()+ylim(-0.75,0.5)

#cauchy分布からのサンプリング
set.seed(1234)
theta0 <- 1.5
x <- rnorm(theta0,1)

f <- function(theta){
  return(theta*exp(-((x-theta)^2)/2))
}

g <- function(theta){
  return(exp(-((x-theta)^2)/2))
}

M <- 1000

y <- numeric(0)
z<- numeric(0)
u <- rcauchy(M,0,1)

for(N in 1:M){
  t <- u[1:N]
  fseq <- as.numeric(lapply(t,f))
  gseq <- as.numeric(lapply(t,g))
  fmean <- mean(fseq)
  gmean <- mean(gseq)
  fvar <- sd(fseq)^2
  gvar <- sd(gseq)^2
  fgcov <- cov(fseq,gseq)
  y <- append(y,fmean/gmean)
  z <- append(z,tail(y,1)^2* (fvar/fmean^2-2*fgcov/fmean*gmean+gvar/gmean^2)/N )
}

data.fr <- data.frame(x=1:M,y=y,z=z)

ggplot(data.fr,aes(x=x,y=y))+geom_line(color="blue",size=2)+
  geom_ribbon(aes(ymin=y-1.96*sqrt(z),ymax=y+1.96*sqrt(z)),alpha=0.3,fill="blue")+theme_bw()+ylim(-0.75,0.5)

#例3.10
set.seed(1234)

f <- function(x){
  return(exp(x/3))
}

r <- function(x){
  return((1/2)*exp(x))
}

fr <- function(x){
  return( exp(x*(4/3))/2 )
}

M <- 5000
x_1 <- rexp(M,1)
y_1 <- numeric(0)
z_1 <- numeric(0)

for(N in 1:M){
  u <- x_1[1:N]
  y_1 <- append(y_1, sum(as.numeric(lapply(u,f)))/N )
  z_1 <- append(z_1, sum( (as.numeric(lapply(u,f)) - sum(as.numeric(lapply(u,f)))/N  )^2 )/(N-1)/N)
}


data.fr1 <- data.frame(x=1:5000,y=y_1,z=z_1)

x_2 <- rexp(M,2)
y_2 <- numeric(0)
z_2 <- numeric(0)

for(N in 1:M){
  u <- x_2[1:N]
  y_2 <- append(y_2, sum(as.numeric(lapply(u,fr)))/N )
  z_2 <- append(z_2,  sum(  (as.numeric(lapply(u,fr)) - sum(as.numeric(lapply(u,fr)))/N )^2)/(N-1)/N )
}
data.fr2 <- data.frame(x=1:5000,y=y_2,z=z_2)

ggplot(data.fr1,aes(x=x,y=y))+geom_line(size=2,color="blue")+
  geom_ribbon(aes(ymin=y-1.96*sqrt(z),ymax=y+1.96*sqrt(z)),alpha=0.3,fill="blue")+theme_bw()+ylim(0.5,2.5)
  

ggplot(data.fr2,aes(x=x,y=y))+geom_line(size=2,color="blue")+
  geom_ribbon(aes(ymin=y-1.96*sqrt(z),ymax=y+1.96*sqrt(z)),alpha=0.3,fill="blue")+theme_bw()+ylim(0.5,2.5)


qplot()+
  geom_line(data=data.fr1,aes(x=x,y=y),size=2,color="blue")+
  geom_ribbon(data=data.fr1,aes(x=x,ymin=y-1.96*sqrt(z),ymax=y+1.96*sqrt(z)),alpha=0.3,fill="blue")+
  geom_line(data=data.fr2,aes(x=x,y=y),size=2,color="red")+
  geom_ribbon(data=data.fr2,aes(x=x,ymin=y-1.96*sqrt(z),ymax=y+1.96*sqrt(z)),alpha=0.3,fill="red")+theme_bw()+ylim(0.5,2.5)
  
  
#練習問題
#3.4
f <- function(x){
  return((cos(50*x)+sin(20*x))^2)
}

M <- 100
x <- seq(0,1,length=M)
sum(as.numeric(lapply(x[-1],f))+as.numeric(lapply(x[-M],f)))/(2*(M-1))

#3.5
f <- function(x){
  if(x>2){1}
  else {0}
}

x <- 1:10000
y <- numeric(0)
z <- numeric(0)
u <- rcauchy(max(x))

for(N in x){
  t <- u[1:N]
  y <- append(y,sum(as.numeric(lapply(t,f)))/N)
  z <- append(z,sum((as.numeric(lapply(t,f))-sum(as.numeric(lapply(t,f)))/N)^2)/(N-1)/N)
}

data.fr <- data.frame(x=x,y=y,z=z)

ggplot(data.fr,aes(x=x,y=y))+geom_line(size=2)+geom_ribbon(aes(ymin=y-1.96*sqrt(z),ymax=y+1.96*sqrt(z)),alpha=0.3)+
  geom_hline(yintercept=0.1476,lty=2)+theme_bw()+ylim(-0.1,0.5)

paste("estimete",y[10000],"lower_boumd",y[10000]-1.96*z[10000],"upper_bound",y[10000]+1.96*z[10000])
  
  
#3.6
N <- 1000
k <- 1000
x <- rcauchy(N)
f <- ecdf(x)  #乱数から経験分布に

points <- seq(-2,2,length=k)
data.fr <- data.frame(x=points,y=pi*(f(points)-0.5))

ggplot(data.fr,aes(x=x,y=y,color="MC"))+
  geom_step(aes(color="direct"))+stat_function(fun=atan)+
  theme_bw()+scale_color_brewer(palette="Set1")

#3.7
#runif method
f1 <- function(x){
  if(x>atan(2)){1}
  else {0}
}

x1 <- 1:5000
y1 <- numeric(0)
z1 <- numeric(0)
u1 <- runif(max(x1),-pi/2,pi/2)

for(N in x1){
  t1 <- u1[1:N]
  y1 <- append(y1,sum(as.numeric(lapply(t1,f1)))/N)
  z1 <- append(z1,sum((as.numeric(lapply(t1,f1))-sum(as.numeric(lapply(t1,f1)))/N)^2)/(N-1)/N)
}

data.fr <- data.frame(x=x1,y=y1,z=z1)

ggplot(data.fr,aes(x=x1,y=y1))+geom_line(size=2)+geom_ribbon(aes(ymin=y1-1.96*sqrt(z1),ymax=y1+1.96*sqrt(z1)),alpha=0.3)+
  geom_hline(yintercept=0.5-atan(2)/pi,lty=2)+theme_bw()+ylim(-0.1,0.5)

#cauchy method
f2 <- function(x){
  if(x>2){1}
  else {0}
}

x2 <- 1:5000
y2 <- numeric(0)
z2 <- numeric(0)
u2 <- rcauchy(max(x2))

for(N in x2){
  t2 <- u2[1:N]
  y2 <- append(y2,sum(as.numeric(lapply(t2,f2)))/N)
  z2 <- append(z2,sum((as.numeric(lapply(t2,f2))-sum(as.numeric(lapply(t2,f2)))/N)^2)/(N-1)/N)
}

data.fr <- data.frame(x=x2,y=y2,z=z2)

ggplot(data.fr,aes(x=x2,y=y2))+geom_line(size=2)+geom_ribbon(aes(ymin=y2-1.96*sqrt(z2),ymax=y2+1.96*sqrt(z2)),alpha=0.3)+
  geom_hline(yintercept=0.5-atan(2)/pi,lty=2)+theme_bw()+ylim(-0.1,0.5)

y1[5000]
y2[5000]
z1[5000]
z2[5000]

#3.8
M <- 1e3
N <- 10
theta0 <- 2
x <- theta0+rcauchy(N)

f <- function(theta){prod(dcauchy(x-theta))*theta}
g <- function(theta){prod(dcauchy(x-theta))}

y <- numeric(0)
z <- numeric(0)
theta <- rcauchy(M)

for(N in 1:M){
  t <- theta[1:N]
  fseq <- as.numeric(lapply(t,f))
  gseq <- as.numeric(lapply(t,g))
  fmean <- mean(fseq)
  gmean <- mean(gseq)
  fvar <- sd(fseq)^2
  gvar <- sd(gseq)^2
  fgcov <- cov(fseq,gseq)
  y <- append(y,fmean/gmean)
  z <- append(z,tail(y,n=1)^2*(fvar/fmean^2-2*fgcov/(fmean*gmean)+gvar/gmean^2)/N)
}

data.fr <- data.frame(x=1:M,y=y,z=z)

ggplot(data.fr,aes(x=x,y=y))+geom_line(color="blue",size=2)+
  geom_ribbon(aes(ymin=y-1.96*sqrt(z),ymax=y+1.96*sqrt(z)),alpha=0.3,fill="blue")+theme_bw()

#3.9
M <- 1e3
x <- 1:M
fr1 <- function(x){
  return((sin(x))^2*pi*(1+x^2)*exp(-sqrt(abs(x))))
}

y1 <- numeric(0)
z1 <- numeric(0)
u1 <- rcauchy(M)

for(N in 1:M){
  t1 <- u1[1:N]
  seq1 <- as.numeric(lapply(t1,fr1))
  mean1 <- mean(seq1)
  y1 <- append(y1,mean1)
  z1 <- append(z1,sum( (as.numeric(lapply(t1,fr1)) - mean1)^2)/(N-1)/N)
}


fr2 <- function(x){
  return(sqrt(2*pi)*exp((x^2)/2-sqrt(abs(x)))*(sin(x))^2)
}

y2 <- numeric(0)
z2 <- numeric(0)
u2 <- rnorm(M)

for(N in 1:M){
  t2 <- u2[1:N]
  seq2 <- as.numeric(lapply(t2,fr2))
  mean2 <- mean(seq2)
  y2 <- append(y2,mean2)
  z2 <- append(z2,sum( (as.numeric(lapply(t2,fr2)) - mean2)^2)/(N-1)/N)
}

data.fr <- data.frame(x=rep(x,2),y=c(y1,y2),z=c(z1,z2),method=rep(c("Cauchy","Normal"),each=M))

ggplot(data=data.fr,aes(x=x,y=y,color=method))+geom_line(size=1.8)+
  geom_ribbon(aes(ymin=y+1.96*sqrt(z),ymax=y-1.96*sqrt(z)),alpha=0.3)+theme_bw()

