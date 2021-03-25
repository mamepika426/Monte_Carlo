library(plotly)
#List2.1 一様乱数の生成
set.seed(1)
runif(3)
set.seed(1)
runif(3)
set.seed(2)
runif(3)

#List2.2 コルモゴロフ・スミルノフ検定
set.seed(1)
u <- runif(1000)
ks.test(u,punif)
u

RNGkind()

#List2.4 RANDU法
x <- numeric(1e2)
y <- 1234

n <- 2^31 -1
a <- 65539
b <- 0

for(i in 1:length(x)){
  y <- (a*y+b)%%n
  x[i] <- y/n
}

x[1:3]
ks.test(x,punif)

#List2.5
set.seed(1)
lambda <- 2
u <- runif(3)
-(log(u))/lambda
set.seed(1)
rexp(3,rate=lambda)

#List2.6 指数乱数の実行時間の比較
install.packages("microbenchmark")
library(microbenchmark)
lambda <- 2
n <- 1e5
microbenchmark(A <- (log(runif(n)))/lambda,times=100)
microbenchmark(B <- rexp(n,rate=lambda),times=100)

#List2.7 コーシー乱数の生成法
n <- 1e5
microbenchmark(A <- tan(pi*(runif(n)-1/2)),times=100)
microbenchmark(B <- rnorm(n)/rnorm(n),times=100)
microbenchmark(C <- rcauchy(n),times=100)


plot(0:10,dgeom(0:10,prob=0.2),type="h")
plot(0:10,pgeom(0:10,prob=0.2),type="s")
curve(pgeom(x,prob=0.2),-1,2,type="l",lwd=2,col=1)

#List2.8 幾何乱数の生成法
p <- 0.2
n <- 5
as.integer(log(runif(n))/log(1-p))
rgeom(n,prob=p)
microbenchmark(A<-as.integer(log(runif(n))/log(1-p)),times=100)
microbenchmark(B <- rgeom(n,prob=p),times=100)

#List2.4
lambda <- 2

rpoisf <- function(n,lambda){
  c <- exp(-lambda)
  res <- numeric(n)
  
  for(i in 1:n){
    x <- 0
    q <- c
    F <- c
    u <- runif(1)
    while(F<u){
      x <- x+1
      F <- F + q*lambda/x
      q <- q*lambda/x
    }
    res[i] <- x
  }
  return(res)
  }
n <- 1e5
microbenchmark(A<-rpoisf(n,lambda),times=100)
microbenchmark(B<-rpois(n,lambda),times=100)

#List2.10 近似累積分布関数による乱数生成
set.seed(1)
m <- 3
set.seed(1)
rnorm(m)
set.seed(1)
qnorm(c(runif(2*m)[seq(from=1,to=2*m-1,by=2)]))

#List2.11 変数変換法によるベータ分布の生成
shape1 <- 2
shape2 <- 3
rbetar <- function(n,shape1,shape2){
  x <- apply(-log(matrix(runif(n*shape1),ncol=n)),2,sum)
  y <- apply(-log(matrix(runif(n,shape2),ncol=n)),2,sum)
  x/x+y
}

rbetar(3,shape1,shape2)

#List2.12 標準正規乱数の生成
n <- 1e5
RNGkind(normal.kind='default')A
microbenchmark(A <- rnorm(n),times=100)

RNGkind(normal.kind='Box-Muller')
microbenchmark(B <- rnorm(n),times=100)

#List 2.13 棄却法によるベータ乱数の生成
alpha <- 2.5
beta <- 3

R <- ( (alpha-1)/(alpha+beta-2) )^(alpha-1)*( (beta-1)/(alpha+beta-2) )^(beta-1)/beta(alpha,beta)

rbetar <- function(n){
  z <- numeric(n)
  for(i in 1:n){
    u <- runif(1);y <- runif(1);
    while(R*u > dbeta(y,shape1=alpha,shape2=beta)){
      u <- runif(1);y<-runif(1);
    }
    z[i] <- y ## zに乱数格納
  }
  return(z)
}

rbetar(3)

#棄却法による乱数をbeta(2.5,3)と比較
fig <- plot_ly(x=rbetar(1e4),type="histogram")
x <- seq(0,1,length=100)
y <- dbeta(x,shape1=alpha,shape2=beta)
df <- data.frame(x=x,y=y)
fig <- fig %>% add_trace(df,x=~x,y=~y,type="scatter",mode="l")


fig

library(ggplot2)
library(reshape2)
x <- rbetar(1e4)

qplot(x,geom="blank") +
  geom_histogram(aes(y= ..density..),fill="dodgerblue",colour="black") + #aes(y=..density..)でヒストグラムをスケールダウン
  stat_function(fun=dbeta,color="brown4",size=2,args=list(shape1=2.5,shape2=3))

#採択されるまでの期待回数のヒートマップ

##ggplot版
alpha <- rep(1:10,10)
beta <- rep(1:10,each=10)
alpha
beta
z <- ( (alpha-1)/(alpha+beta-2) )^(alpha-1)*( (beta-1)/(alpha+beta-2) )^(beta-1)/beta(alpha,beta)
z
df <- data.frame(x = alpha,y = beta, value = z )
df
ggplot(df, aes(as.factor(x), y, group=y)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = df$value, label = round(df$value, 1))) +
  scale_fill_gradient(low = "white", high = "red") 


##plot_ly版
plot_ly(x=alpha,y=beta,
        z=( (alpha-1)/(alpha+beta-2) )^(alpha-1)*( (beta-1)/(alpha+beta-2) )^(beta-1)/beta(alpha,beta),
        type="contour",
        contours=list(start=0,end=5,size=0.5))

#List 2.14
n <- 1e5
R <- sqrt(2*pi)*exp(-1/2)
rnormr <- function(n){
  z <- numeric(n)
  for(i in 1:n){
    u <- runif(1);y <- tan(pi*runif(1));
    while(R*u>dnorm(y)/dcauchy(y)){
      u <- runif(1);y <- tan(pi*runif(1));
    }
    z[i] <- y
  }
  return(z)
}
d <- rnormr(n)

qplot(d,geom="blank") +
  geom_histogram(aes(y=..density..),fill="dodgerblue",colour="black")+
  stat_function(fun=dnorm,color="brown4",size=2,args=list(mean=0,sd=1))

#List 2.15
dgammar <- function(n,nu){
  if(nu==1){
    return(-log(runif(n)))
  }
  else if(nu==as.integer(nu)){
    return(apply(matrix(-log(runif(n*nu),ncol=nu)),1,sum))
  }
  else{
    inu <- as.integer(nu)
    R <- 2^(inu)*(2*(nu-inu))^(nu-inu)*exp(-(nu-inu))
    z <- numeric(n)
    for(i in 1:n){
      u <- runif(1);y <- 2*sum(-log(runif(inu)));
      while(R*u>2^(inu)*y-(nu-inu)*exp(-y/2)){
        u <- unif(1);y <- 2*sum(-log(runif(inu)));
      }
      z[i] <- y
    }
    return(z)
  }
}
nu <- 5.2
dgammar(3,nu)

###練習問題###
#2.1
z <- runif(1000)
x <- z[seq(1,length(z),by=2)]
y <- z[seq(2,length(z),by=2)]
df <- data.frame(x=x,y=y)

ggplot(data=df,mapping=aes(x=x,y=y))+
  geom_point()

#2.2
a <- 13
b <- 0
n <- 67
y <- 1234

z <- numeric(1000)
for(i in 1:1000){
  y <- (a*y+b)%%n
  z[i] <- y/n
}
z
x <- z[seq(1,length(z),by=2)]
y <- z[seq(2,length(z),by=2)]
df <- data.frame(x=x,y=y)

ggplot(data=df,mapping=aes(x=x,y=y))+
  geom_point()

#2.3
n <- 1e5
f <- function(u){return(log(u)-log(1-u))}
logisr <- function(n){
  z <- numeric(n)
  x <- runif(n)
  for(i in 1:n){
    z[i] <- log(x[i])-log(1-x[i])
  }
  return(z)
}

logis_gen <- logisr(n)

microbenchmark(A <- f(runif(n)),times=100)
microbenchmark(B <- rlogis(n),times=100)

#2.4
n <- 1e5
a <- 2
b <- 1
paretor <- function(n){
  z <- numeric(n)
  rand <- runif(n)
  for(i in 1:n){
    z[i] <- b/(rand[i])^(1/a)
  }
  return(z)
}
f <- function(x) a*b^a/(x^(a+1))
pareto_gen <- paretor(n)
qplot(pareto_gen,geom="blank")+
  geom_histogram(aes(y=..density..),fill="dodgerblue",colour="black",binwidth = 0.1)+
  theme_bw()+xlim(1,4)+stat_function(fun=f)

#2.5
n <- 1e5
sigma <- 2
f <- function(x){return(x/(sigma^2)*exp(-x^2/(2*sigma^2)))}
rayleighr <- function(n){
  z <- numeric(n)
  x <- runif(n)
  for(i in 1:n){
    z[i] <- sqrt(-2*(sigma^2)*log(x[i]))
  }
  return(z)
}

rayleigh_gen <- rayleighr(n)
qplot(rayleigh_gen,geom="blank")+
  geom_histogram(aes(y=..density..),fill="dodgerblue",colour="black")+
  stat_function(fun=f)

#List2.6





