library(pracma)
colors = c("#00ced1","#ffa500","#00ff00","#0000ff","#ff1493") 
set.seed(12345)

wiener <- function(n, dt) {
  w <- rep(NA, n+1) # rep() funkcija pakartoja NA reikðmæ n+1 kartø
  w[1] <- 0 # pirmasis Veinerio proceso skaièius yra 0
  for (i in 1:n) {
    dw <- sqrt(dt) * rnorm(1) # rnorm(1) sugeneruoja 1 skaièiø ið distribucijos N(0,1)
    w[i+1] <- w[i] + dw # kaupiama veinerio proceso seka
  } 
  w # graþinamas rezultatas
}

t<- linspace(0,10, n = 1000)
head(t)

tail(t)

d <- t[2] - t[1];d

w <- wiener(1000-1, d)
head(w)
tail(w)

xt<- (w + t + 0.5) - 0.5
head(xt)
tail(xt)

plot(t,xt, type = "l", xlab = "t", ylab = "X(t) = W(t + 0.5) - W(0.5)", col= colors[1])

skirtumai <- diff(xt)
mean(skirtumai)

sd(skirtumai)

hist(skirtumai, breaks = 20)

for(i in 1:5)
{
  t<- linspace(0,10, n = 1000)
  d <- t[2] - t[1]
  w <- wiener(999, d)
  xt<- (w + 0.5) - 0.5
  if(i == 1)
    plot(t,xt, type = "l", xlab = "t", ylab = "X(t) = W(t + 0.5) - W(0.5)", col= colors[i], lwd = 2, ylim = c(-10,10))
                  else
                    lines(t,xt, type = "l", xlab = "t", ylab = "X(t) = W(t + 0.5) - W(0.5)", col= colors[i], lwd = 2, ylim =c(-10,10))
                    
  
}
                  
