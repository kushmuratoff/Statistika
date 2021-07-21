#first task
#this function for Variance
myVar<-function(x)
{
  
  return(sqrt(mean((x-mean(x))^2)))
}

#this function for Correlation
myCorr<-function(x,y)
{
  sx <- myVar(x)
  sy <- myVar(y)
  return((mean(x*y)-mean(x)*mean(y))/(sx*sy))
}

myLm<-function(x,y)
{
  sx <- myVar(x)
  
  sy <- myVar(y)
  
  rxy<-myCorr(x,y)
  
  b1<-rxy*(sy/sx)
  b1
  
  b0<-mean(y)-b1*mean(x)
  b0
  b<-c(b0,b1)
  return(b)
  
}

myS2<-function(X,Y,mycoe)
{
  ss<- sum((Y -(mycoe[1]+X*mycoe[2]))^2)
  return( ss)
}

#students <- read.csv("students.csv")
#print(students)
#names(students) <- c("x", "y")

Y<-c(13,18,10,11,12,11,10,14)
X<-c(43,40,45,48,42,44,50,42)

plot(X,Y,col="red",xlab="x",ylab="y")

sx <- myVar(X)
sx

sy <- myVar(Y)
sy

rxy<-myCorr(X,Y)
rxy

#pairwise regression
R2<-rxy^2
R2

b_arr<-myLm(X,Y)
b_arr

plot(X,Y,col="red",xlab="x",ylab="y")
lines(X,b_arr[1]+b_arr[2]*X,col="green", type = "l")

ss<- sqrt(myS2(X,Y,b_arr)/(length(X)-2))
ss

s0<-(1/length(X))* (ss/sx)* sqrt((1/length(X)) * sum(X^2))
s0

s1<- 1/(length(X))* ss/sx
s1

t<-2.44

b0_down <- b_arr[1]-t*s0
b0_down

b0_up<-b_arr[1]+t*s0
b0_up

b1_down <- b_arr[2]-t*s1
b1_down

b1_up<-b_arr[2]+t*s1
b1_up

df <- data.frame(X,Y)
df

fit<-lm(Y ~ X ,data=df)
summary(fit)

confint(fit)














#second task
rxy1<-myCorr(students[,c("x")]*1.2,students[,c("y")]-1)
rxy1

#compare own function and cor
corrr<-cor(students[,c("x")]*1.2,students[,c("y")]-1)
corrr

#pairwise regression
R2<-rxy^2
R2

#third task





#fourth task

myAnsc<-function(x,y)
{
  mx <- mean(x)
  sx<-myVar(x)^2
  my <- mean(y)
  sy<-myVar(y)^2
  rxy<-myCorr(x,y)
  lm<-myLm(x,y)
  r2<-rxy^2
  result<-c(mx,sx,my,sy,rxy,lm,r2)
  return(result)
  
}

data<-anscombe

result1<-myAnsc(data[,c("x1")],data[,c("y1")])
result1
plot(data[,c("x1")],data[,c("y1")],col="red",xlab="x1",ylab="y1")
lines(data[,c("x1")],result1[6]+result1[7]*data[,c("x1")],col="green")


result2<-myAnsc(data[,c("x2")],data[,c("y2")])
result2
plot(data[,c("x2")],data[,c("y2")],col="red",xlab="x2",ylab="y2")
lines(data[,c("x2")],result2[6]+result2[7]*data[,c("x2")],col="green")


result3<-myAnsc(data[,c("x3")],data[,c("y3")])
result3
plot(data[,c("x3")],data[,c("y3")],col="red",xlab="x3",ylab="y3")
lines(data[,c("x3")],result3[6]+result3[7]*data[,c("x3")],col="green")



result4<-myAnsc(data[,c("x4")],data[,c("y4")])
result4
plot(data[,c("x4")],data[,c("y4")],col="red",xlab="x4",ylab="y4")
lines(data[,c("x4")],result4[6]+result4[7]*data[,c("x4")],col="green")


#--------------------------------
ansX <- anscombe[["x1"]]
ansY <- anscombe[["y1"]]
fit<-lm(ansY ~ ansX)
summary(fit)

#Std error

n <- length(ansY)
y <- matrix(ansY, ncol = 1)
y
# Add column of 1s to the `ansX` data for \beta_0
X <- matrix(c(rep(1, n), ansX), ncol = 2)
X
beta <- matrix(fit$coefficients, ncol = 1)
beta
p <- length(beta)
p
varianceEstimate <- sum((y - X %*% beta)^2) / (n - p)
varianceEstimate
covMatrix <- varianceEstimate * solve(t(X) %*% X)
covMatrix
sqrt(diag(covMatrix))

sqrt(varianceEstimate)

#F-statistic

C <- matrix(c(0, 1), ncol = 2)
d <- 0
side <- C %*% beta - d
D.SSE <- t(side) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% side
SSE <- t(y - X %*% beta) %*% (y - X %*% beta)
(F.stat <- (D.SSE/SSE) * (n - p))

sqrt(F.stat)

#multiple regression 

cognitive <- read.csv("cognitive.csv")
cognitive

fit <- lm(kid_score ~ mom_iq + mom_age, data = cognitive)
summary(fit)


fit.alt <- lm(kid_score ~ mom_hs + mom_work, data = cognitive)
summary(fit.alt)

#about twins

twins <- read.csv("twins.csv")
twins
fit.twins <- lm(Foster ~ Biological + Social, data = twins)
summary(fit.twins)


# Forward selection with Adjusted R-squared

fit.full <- lm(kid_score ~ mom_hs + mom_iq + mom_work + mom_age, data = cognitive)
summary(fit.full)
1 - (1 - 0.2171) * ((434 - 1)/(434 - 4 - 1))
#length(cognitive[,c("kid_score")])

summary(lm(kid_score ~ mom_hs, data = cognitive))
summary(lm(kid_score ~ mom_iq, data = cognitive))
summary(lm(kid_score ~ mom_work, data = cognitive))
summary(lm(kid_score ~ mom_age, data = cognitive))

summary(lm(kid_score ~ mom_iq + mom_hs, data = cognitive))
summary(lm(kid_score ~ mom_iq + mom_work, data = cognitive))
summary(lm(kid_score ~ mom_iq + mom_age, data = cognitive))

summary(lm(kid_score ~ mom_iq + mom_hs + mom_work, data = cognitive))
summary(lm(kid_score ~ mom_iq + mom_hs + mom_age, data = cognitive))

summary(lm(kid_score ~ mom_iq + mom_hs + mom_work + mom_age, data = cognitive))


#Backwards elimination with p-values

summary(lm(kid_score ~ mom_iq + mom_hs + mom_work + mom_age, data = cognitive))


summary(lm(kid_score ~ mom_iq + mom_hs + mom_work, data = cognitive))

summary(lm(kid_score ~ mom_iq + mom_hs, data = cognitive))


#task 5
print("dddddddddddddddd")
summary(lm(kid_score ~ mom_hs, data = cognitive))
summary(lm(kid_score ~ mom_iq, data = cognitive))
summary(lm(kid_score ~ mom_work, data = cognitive))
summary(lm(kid_score ~ mom_age, data = cognitive))
# in my solve the smallest variable at mom_iq

summary(lm(kid_score ~ mom_iq + mom_hs, data = cognitive))
summary(lm(kid_score ~ mom_iq + mom_work, data = cognitive))
summary(lm(kid_score ~ mom_iq + mom_age, data = cognitive))
# in my solve the smallest variable at mom_iq+mom_hs
summary(lm(kid_score ~ mom_iq + mom_hs + mom_work, data = cognitive))
summary(lm(kid_score ~ mom_iq + mom_hs + mom_age, data = cognitive))


#task 6
summary(lm(kid_score ~ mom_iq + mom_hs + mom_work + mom_age, data = cognitive))
#------------

summary(lm(kid_score ~ mom_iq + mom_hs + mom_work, data = cognitive))#the max 0.2109
summary(lm(kid_score ~ mom_iq + mom_hs + mom_age, data = cognitive))
summary(lm(kid_score ~ mom_iq + mom_work + mom_age, data = cognitive))


#------------

summary(lm(kid_score ~ mom_iq + mom_hs, data = cognitive))
summary(lm(kid_score ~ mom_iq + mom_work, data = cognitive))


