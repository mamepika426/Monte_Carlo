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
  geom_line(data=true,aes(x=x,y=true_y),linetype="dashed",color="red",size=2)+
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



