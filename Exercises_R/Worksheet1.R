################### 
### Worksheet 1 ###
###################

# Exercise 2

# (b)

# Simulate series with 500 Gaussian white noise observations
# w_t N(0,1)
# n = 500

w <- rnorm(500)
w
par(mfrow=c(2,1)) # para vermos os dois gráficos
plot(w)

# Make the cronogram
plot.ts(w, ylab = "w_t", main = "Gaussian White Noiese Series", ylim = c(-3, 3))

# Sample ACF
acf(w) # era o esperado, porque quando h=0 temos 1 e quando h!=0 temos valores
# muito próximos de 0 (entre (+/-)1.96/sqrt(n) = (+/-)1.96/sqrt(500))

# ^p(20)
print(acf(w)) # prints all values

acf(w,lag=20,plot=FALSE)  # prints only until the 20th lag.

# Neste caso, obtivamos um valor de exatamente 0, que é exatramente o valor
# da ACF teórica.


# (c)

# Agora fazemos o mesmo, mas para n=50
# Não se espera grande mudança, porque os valores são independentes e, portanto,
# não vão depender do n.


w <- rnorm(50)
w
par(mfrow=c(2,1))
plot(w)

# Make the cronogram
plot.ts(w, ylab = "w_t", main = "Gaussian White Noiese Series", ylim = c(-3, 3))

# Sample ACF
acf(w) # era o esperado, porque quando h=0 temos 1 e quando h!=0 temos valores
# muito próximos de 0 (entre (+/-)1.96/sqrt(n) = (+/-)1.96/sqrt(50))

# ^p(20)
print(acf(w)) # prints all values

acf(w,lag=20,plot=FALSE)  # prints only until the 20th lag.

# Obtivemos p^(20)=-0.031 e o valor teórico seria p(20).
# A nível da ACF os resultados estão bastantes próximos.

###############################################################################

# Exercise 3

# (b)

set.seed(123) #reproducibility

n=500
w <- rnorm(n)

# Criamos um vetor vazio x_t com 498 espaços já que temos t a variar de 2 a 500
x <- numeric(n-2)

# Agora simulamos x_t para t=2,3,...,n
for (t in 2:(n-1)){
  x[t-1] <- (1/3)*(w[t-1]+w[t]+w[t+1])
}

# Make the cronogram
plot.ts(x, ylab = "x_t", main = "Smoothed White Noise Process")

# Sample ACF
acf(w) # era o esperado, porque quando h=0 temos 1 e quando h!=0 temos valores
# muito próximos de 0 (entre (+/-)1.96/sqrt(n) = (+/-)1.96/sqrt(500))


# (c)
acf(x,lag=20, plot=FALSE) # o valor teórico é 0, o valor da amostra é -0.022


# (d)
# Alterando o tamanho da amostra,não esperamos uma grande alteração da ACF, uma 
# vez que os valores são independentes e, portanto, o n não deve alterar o valor
# da ACF

set.seed(123) #reproducibility

n=50
w <- rnorm(n)

# Criamos um vetor vazio x_t com 498 espaços já que temos t a variar de 2 a 500
x <- numeric(n-2)

# Agora simulamos x_t para t=2,3,...,n
for (t in 2:(n-1)){
  x[t-1] <- (1/3)*(w[t-1]+w[t]+w[t+1])
}

# Make the cronogram
plot.ts(x, ylab = "x_t", main = "Smoothed White Noise Process")

# Sample ACF
acf(w) # era o esperado, porque quando h=0 temos 1 e quando h!=0 temos valores
# muito próximos de 0 (entre (+/-)1.96/sqrt(n) = (+/-)1.96/sqrt(500))


acf(x,lag=20, plot=FALSE) # o valor teórico é 0, o valor da amostra é -0.022

###############################################################################

# Exercício 3 (na aula)

# (b) 

### x_t = 1/3(w_t-1+w_t+w_t+q), w_t N(0,1)
### gerar 502 dados (porque começamos em 2 e acabamos em 499)

w <- rnorm(502)
w

plot.ts(w,main="ruído branco gaussiano")

x<- filter(w,sides=2,rep(1/3,3)) #rep, distributes the weight 1/3 in the three gaussians
x

par(mfrow=c(2,1))
plot.ts(w,main="ruido branco gaussiano")
plot.ts(x, main="ruido branco amaciado")

acf(x) ### error, why? notice it is filtered on both sides
par(mfrow=c(1,1))

mdata500=window(x,start=2, end=501) #I need to give a beginning and an end
acf(mdata500)

acf(mdata500, lag=20, plot=FALSE)

# ro estimado= ro(20)=0.016

par(mfrow=c(1,1))

#### generating n=50

w<-rnorm(50)
w
plot.ts(w,main="ruido branco gaussiano")


# Exercicio 4

# Autoregressive process
# x_t= x_t-1-0.9x_t-2+w_t, w_t N(0,1)

# Adicionamos 50 e analisamos só os últimos 500, para não ter em conta a dependencia
# dos valores iniciais

w = rnorm(500)
x=filter(w,filter=c(1,-0.9), method="recursive") #suitable for autoregression
# filter=c(1,-0.9) são as constantes da nossa equacao x_t
# faz diretamente o processo iterativo

w=rnorm(550) # simulam-se 50 a mais para não estar dependente dos valores iniciais
x_todos=filter(w, filter=c(1,-0.9), method="recursive")
x_todos
x=filter(w,filter=c(1,-0.9),method="recursive")[-(1:50)]

##ou
x=window(x_todos, start=51)

plot.ts(x,main="autorregressao")
#### x=as .ts(x) ou x=ts(x) # para tratar de uma série temporal
#### ou plot(ts(x), main="autoregressao")


# amplitude não parece constante, nao parece haver um padrao

par(mfrow=c(2,1))
acf(x)
pacf(x)


acf(x, lag=40)
pacf(x,lag=40)

# (b)

#### changing the autoregressive parameters
#x_t=0.2x_{t-1}-0.4x_{t-2}+w_t, t=1,2, \ldots, 500, w_t \sim N(0,1) rbeta

w=rnorm(550)
x_todos = filter(w, filter=c(0.2,-0.4), method="recursive")

# ou 

x=window(x_todos, start=51)

par(mfrow= c(1,1))
acf(x)
plot.ts

#############
# Exercício 5

## y_t=5+x_t-0.7x_(t-1)


## simulate a sample with n=10

rbinom(11,1,0.5)  #bernoulli; binomial
set.seed(102030)
x_1=2*rbinom(11,1,0.5)-1 # to transform the bernoulli results in -1 or +1
x_1 #sequence with n=10

y_1=5+filter(x_1,sides=1,filter=c(1,-0.7))[-1] #para perder a dependencia do 1º

y_1

par(mfrow=c(2,1))
plot.ts(y_1)
plot.ts(y_1, type='s')

par(mfrow=c(1,1))
acf(y_1, lag.max=6, plot=FALSE)
acf(y_1, lag.max=6)

# nao da bem o esperado (não estão proximos de 0, para h!=0. Talvez pelo tamanho da 
# amostra, por ser muito pequeno. Por isso, vamos tentar fazer para uma amostra
# maior)
sd(y_1)

# Simulating the sample with size n=1000

st.seed(102030)

# aqui já vemos que os valores se aproximam mais de 0. Depende do tamanho da amostra. 

