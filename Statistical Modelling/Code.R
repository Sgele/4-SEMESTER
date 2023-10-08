#1 uzduotis
m1 <- 1168
m2 <- 875
x1 <- c()
x2 <- c()
x1[1] <- 0
x2[1] <- 0

m <- m1
b <- seq(0, 1168 - 73*4, 73*4)
b <- seq(0, 1168 - 73*4, 73*4)
b

(b^2) %% m

b <- b[length(b)]
a <- b + 1

c1 <- 1/2 - sqrt(3)/6
c2 <- 1/2 + sqrt(3)/6

c(m*c1, m*c2)

library(schoolmath)
gcd(246, 1168)
gcd(247, 1168) 
gcd(921, 1168) 
gcd(922, 1168) 

c <- 247

for (i in 1:(m-1)) x1[i+1] <- (a * x1[i] + c) %% m
x1

m <- m2
b <- seq(0, 875 - 7*5, 7*5)
b

(b^2) %% m

(b^3) %% m

a <- b[25] + 1

c(m*c1, m*c2)
gcd(184, m)
gcd(185,m)
gcd(690,m)
gcd(691,m)

for (i in 1:(m-1)) x2[i+1] <- (a * x2[i] + c) %% m
x2

#2 uzduotis
#intervalu testas
t = 5 
U = c() 
alpha = 2/3 
beta = 1
U = x1/m 
pat.int = which((U >= alpha) & (U < beta))  #Kurie sekos nariai priklauso intervalui 
ilgiai = (pat.int - 1) - c(0, pat.int[-length(pat.int)]) 
rez = matrix(0, 1, 7, dimnames = list(c(""),c("0-ilgio", "1-ilgio", "2-ilgio", "3-ilgio", "4-ilgio", "5-ilgio", ">5-ilgio"))) 
rez[1] = sum(ilgiai == 0) 
rez[2] = sum(ilgiai == 1) 
rez[3] = sum(ilgiai == 2) 
rez[4] = sum(ilgiai == 3) 
rez[5] = sum(ilgiai == 4) 
rez[6] = sum(ilgiai == 5) 
rez[7] = sum(ilgiai > 5) 
rez 
p = beta - alpha 
tik = c(p*(1-p)^(0:t), (1-p)^(t+1)) 
chisq.test(rez, p = tik)

U2 = c() 
U2 = x2/m 
pat.int2 = which((U2 >= alpha) & (U2 < beta)) #Kurie sekos nariai priklauso intervalui 
ilgiai2 = (pat.int2 - 1) - c(0, pat.int2[-length(pat.int2)]) 
rez2 = matrix(0, 1, 7, dimnames = list(c(""),c("0-ilgio", "1-ilgio", "2-ilgio", "3-ilgio", "4-ilgio", "5-ilgio", ">5-ilgio"))) 
rez2[1] = sum(ilgiai2 == 0) 
rez2[2] = sum(ilgiai2 == 1) 
rez2[3] = sum(ilgiai2 == 2) 
rez2[4] = sum(ilgiai2 == 3)
rez2[5] = sum(ilgiai2 == 4) 
rez2[6] = sum(ilgiai2 == 5) 
rez2[7] = sum(ilgiai2 > 5) 
rez2 
p = beta - alpha 
tik2 = c(p*(1-p)^(0:t), (1-p)^(t+1)) 
chisq.test(rez2, p = tik2)

#keliniu testas

n <- m1
u <- x1/n
u
k <- 2
maziau <- 0
daugiau <- 0
v1 <- u[-length(u)]
v2 <- u[-1]
maziau <- as.numeric(length(which(v1 < v2)))
maziau
daugiau <- as.numeric(length(which(v1 >= v2)))
d <- c(maziau, daugiau)
d

prob <- c(1/2, 1/2)
chisq.test(d, p=prob)


n <- m2
u <- x2/n
k <- 2
maziau <- 0
daugiau <- 0
v1 <- u[-length(u)]
v2 <- u[-1]
maziau <- as.numeric(length(which(v1 < v2)))
daugiau <- as.numeric(length(which(v1 > v2)))
d <- c(maziau, daugiau)
d

prob <- c(1/2, 1/2)
chisq.test(d, p=prob)

#3 uzduotis
U=x2/m2

V=c()
for (i in 1:m2){
  V[i]=2*U[i]-1
}

X=c();L=c();S=c();r=1;i=1

while (i<m2){
  S[i]=V[i]^2+V[i+1]^2
  if (S[i] >= 1) i=i+1 else {	 
    L[r]=logb(S[i], base = exp(1))
    X[r]=V[i]*((-2*L[r])/(S[i]))^(1/2)
    r=r+1
    i=i+1}
}

Y=0.5+sqrt(0.5)*X
Y
mean(Y)
var(Y)

plot(density(Y))
jarque.bera.test(Y) 

E <- - 4 * log(u[- which(u == 0)])
plot(density(E))

#4 uzduotis
f <- function(z) {(z^4-z)/(z^2+1)}
integrate(f,0,7)

plot(f, xlim=c(0,7))

n <- m2
u <- 7 * x2 / n

I <- mean(f(u))*7
I
plot(f(sort(u)), type="l")

#5
mcsd <- c(1/6, 2/6, 3/6, 0)
P <- matrix(rep(mcsd, 4), ncol=4, byrow=TRUE)
u <- x2/m2
b <- c()
b[1] <- 1
for (i in 1:(m2-1)){
  if (u[i] <= 0.16666667) {b[i+1] <- 1}
   else {if (u[i] <= 0.5) {b[i+1] <- 2}
      else {b[i+1] <- 3}}
  	}
b


