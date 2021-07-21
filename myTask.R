#
mydata <- read.csv("MyData.csv")
mydata

myVar<-function(x){
  return(length(x)*sum(x^2)-sum(x)^2)
 
}

myPearson <- function(x,y){
  
  a<-length(x)*sum(x*y)-sum(x)*sum(y)
 
  return(a/sqrt(myVar(x)*myVar(y)))
  
}

#1 task
#Найти коэффициент корреляции (Пирсона) между средним баллом и заработком.
mydata["Means"]<- rowMeans(mydata[,2:18])
mydata
my_corr<-myPearson(mydata[,c("Salary")],mydata[,c("Means")])
my_corr
#0.6445952
cor_coeff<- cor(mydata[,c("Salary")],mydata[,c("Means")], method = "pearson")
cor_coeff
#0.6445952
#my_corr^2
#0.415503


#------------------------------
#2 task
#Определите 4 наиболее связанных с величиной зарплаты предмета с помощью
#прямого и обратного методов отборапеременных по p-value для линейной регрессии
#(место проживания учитывайте как фиксированный эффект).





#FORWARD

new_df <- mydata[c(-1,-19,-21)]
names(new_df)
#forward data frame begins Salary
forward_df <- mydata[20]
names(forward_df)

#we must find the smallest p - value in each step
# in first step we will find smallest p value between Salary and other variables  

#
need_df <- forward_df
small_p_val <- 1

other_names <- names(new_df[!(names(new_df) %in% c(names(forward_df)))])
print(other_names)
for (name in other_names) {
  df <- new_df[c(names(forward_df), name)]
  print(names(df))
  fit <- lm(Salary ~ ., data = df)
  #print(summary(fit))
  val <- summary(fit)$coefficients[,4][name]
  print((val))
  
    if (val < small_p_val) {
      small_p_val <- val
      need_df <- df
    }
}
print(names(need_df))
# after fist step we know that the smallest p value in "Chemistry"= 1.121481e-107
# but we must compare this value to "Region"
fit <- lm(Salary ~ Region, data = mydata)
summary(fit)
# Region is factor so it has two kind of variables
#Regionnorth 0.00656 and Regionsouth 0.41995
# but both p values bigger than p value of "Chemistry" 
# so in first we got Salary ~ Chemistry


#second step

#-----------
forward_df <- need_df
names(forward_df)
need_df2 <- forward_df
names(need_df2)
small_p_val2 <- 1
other_names <- names(new_df[!(names(new_df) %in% c(names(forward_df)))])
print(other_names)

for (name in other_names) {
  df <- new_df[c(names(forward_df), name)]
  print(names(df))
  fit <- lm(Salary ~ ., data = df)
  #print(summary(fit))
  val <- summary(fit)$coefficients[,4][name]
  print(val)
  if (val < small_p_val2) {
    small_p_val2 <- val
    need_df2 <- df
  }
}
print(names(need_df2))

# after second step we know that the smallest pvalue in "Literature" 4.742281e-06
# p-value=4.742281e-06
# we must again check Region
fit <- lm(Salary ~ Chemistry+Region, data = mydata)
summary(fit)
#Regionnorth 0.000355 and Regionsouth 0.465968    
# but both p values bigger than p value of "Literature" 
# so in first we got Salary ~ Chemistry + Literature


#3- step

forward_df <- need_df2
names(forward_df)
need_df3 <- forward_df
names(need_df3)
small_p_val3 <- 1
other_names <- names(new_df[!(names(new_df) %in% c(names(forward_df)))])
print(other_names)

for (name in other_names) {
  df <- new_df[c(names(forward_df), name)]
  print(names(df))
  fit <- lm(Salary ~ ., data = df)
  #print(summary(fit))
  val <- summary(fit)$coefficients[,4][name]
  print(val)
  if (val < small_p_val3) {
    small_p_val3 <- val
    need_df3 <- df
  }
}
print(names(need_df3))
# after third step we know that the smallest pvalue in "Geometry" 1.894307e-09
# p-value=1.894307e-09 
# we must again check Region
fit <- lm(Salary ~ Chemistry+Literature+Region, data = mydata)
summary(fit)
#Regionnorth 0.000385 and Regionsouth 0.396507        
# but both p values bigger than p value of "Geometry" 
# so in first we got Salary ~ Chemistry + Literature + Geometry

#4-step

forward_df <- need_df3
names(forward_df)
need_df4 <- forward_df
names(need_df4)
small_p_val4 <- 1
other_names <- names(new_df[!(names(new_df) %in% c(names(forward_df)))])
print(other_names)

for (name in other_names) {
  df <- new_df[c(names(forward_df), name)]
  print(names(df))
  fit <- lm(Salary ~ ., data = df)
  #print(summary(fit))
  val <- summary(fit)$coefficients[,4][name]
  print(val)
  if (val < small_p_val4) {
    small_p_val4 <- val
    need_df4 <- df
    }
}
print(names(need_df4))
# after third step we know that the smallest pvalue in "English" 3.674959e-05 
# p-value=3.674959e-05 
# we must again check Region
fit <- lm(Salary ~ Chemistry+Literature+Geometry+Region, data = mydata)
summary(fit)
#Regionnorth 0.000256  and Regionsouth 0.329703            
# but both p values bigger than p value of "English" 
# so in first we got Salary ~ Chemistry + Literature + Geometry+English


# BACKWARD


new_df <- mydata[c(-1,-21)]
names(new_df)
#backward data frame begins all variables
backward_df <- names(new_df[!(names(new_df) %in%"")])
print(backward_df)

#we must find the biggest p - value in each step and delete it
df <- new_df[c(backward_df)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#Music 9.788345e-01



#2step
# in here we delete Music
backward_df2 <- backward_df[!backward_df %in% "Music"]
print(backward_df2)
df <- new_df[c(backward_df2)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
# Art 9.418982e-01

#3 step
backward_df4 <- backward_df2[!backward_df2 %in% "Art"]
print(backward_df4)
df <- new_df[c(backward_df4)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#History 8.848348e-01

#5 step
backward_df5 <- backward_df4[!backward_df4 %in% "History"]
print(backward_df5)
df <- new_df[c(backward_df5)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#Social.Studies 6.605001e-01

#6 step
backward_df6 <- backward_df5[!backward_df5 %in% "Social.Studies"]
print(backward_df6)
df <- new_df[c(backward_df6)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#Algebra 4.387508e-01

#7 step
backward_df7 <- backward_df6[!backward_df6 %in% "Algebra"]
print(backward_df7)
df <- new_df[c(backward_df7)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#Biology 6.036405e-01

#8 step
backward_df8 <- backward_df7[!backward_df7 %in% "Biology"]
print(backward_df8)
df <- new_df[c(backward_df8)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#Crafts 4.348809e-01

#9 step
backward_df9 <- backward_df8[!backward_df8 %in% "Crafts"]
print(backward_df9)
df <- new_df[c(backward_df9)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#PhysEd 3.165020e-01

#10 step
backward_df10 <- backward_df9[!backward_df9 %in% "PhysEd"]
print(backward_df10)
df <- new_df[c(backward_df10)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#Russian 2.155609e-01

#11 step
backward_df11 <- backward_df10[!backward_df10 %in% "Russian"]
print(backward_df11)
df <- new_df[c(backward_df11)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#in this step we know that Regionsouth is the biggest so we will delete Region
#Region 1.437638e-01

#12 step
backward_df12 <- backward_df11[!backward_df11 %in% "Region"]
print(backward_df12)
df <- new_df[c(backward_df12)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#Informatics 1.676871e-01

#13 step
backward_df13 <- backward_df12[!backward_df12 %in% "Informatics"]
print(backward_df13)
df <- new_df[c(backward_df13)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#Economy 8.116222e-02

#14 step
backward_df14 <- backward_df13[!backward_df13 %in% "Economy"]
print(backward_df14)
df <- new_df[c(backward_df14)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#Physics 5.848504e-03

#15 step
backward_df15 <- backward_df14[!backward_df14 %in% "Physics"]
print(backward_df15)
df <- new_df[c(backward_df15)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)
#Geography 4.467555e-03

#16 step
backward_df16 <- backward_df15[!backward_df15 %in% "Geography"]
print(backward_df16)
df <- new_df[c(backward_df16)]
print(names(df))
fit <- lm(Salary ~ ., data = df)
summary(fit)
val <- summary(fit)$coefficients[,4]
print(val)


#3 task
#Найдите коэффициенты в задаче линейной регрессии для модели
#с ранее выбранными переменными и опишите их смысл.



print(need_df4)
fit<-lm(Salary ~ ., data = need_df4)
summary(fit)$coefficients[,1]
confint(fit)
#Наш результат показывает что если студент получил 0 баллов все эти 4 предметы
#через 15 лет он заработает 26528.4646 долларов за год. Но каждый дополнительные
#баллы дадут дополнительные запрлата
#Chemistry  Literature    Geometry     English 
#231.0758    151.3768   -203.0297    118.5434

#4 task
#С помощью F-критерия проверьте гипотезы о том, что с величинй зарплаты связаны:
#1) какие-либо математические (алгебра, геометрия) дисциплины;
#2) какие-либо естественно-научные (география, биология, физика, химия) дисциплины.




mathSubject<-c('Algebra', 'Geometry')
nat_sciSubject<-c("Geography", "Biology", "Physics", "Chemistry")


for( name in c(mathSubject,nat_sciSubject))
{
  fit <- lm(Salary ~ ., data = mydata[,c(name,"Salary")])
  n <- length(mydata[,c("Salary")])
  y <- matrix(mydata[,c("Salary")], ncol = 1)
  beta <- matrix(fit$coefficients, ncol = 1)
  p <- length(beta)
  X <- matrix(c(rep(1, n), mydata[,c(name)]), ncol = p)
  C <- matrix(c(0, 1), ncol = 2)
  d <- 0
  side <- C %*% beta - d
  D.SSE <- t(side) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% side
  SSE <- t(y - X %*% beta) %*% (y - X %*% beta)
  (F.stat <- (D.SSE/SSE) * (n - p)/(p-1))
  pVal<-pf(F.stat, p-1, n-p, lower.tail = FALSE)
  if(pVal<0.05)
  {
    print(paste("F-статистика для ",name," ", F.stat,
                ', p value = ', pVal, sep = ''))
  }
  
}






#F-статистика для математических наук
fit <- lm(Salary ~ ., data = mydata[,c(mathSubject,"Salary")])
n <- length(mydata[,c("Salary")])
y <- matrix(mydata[,c("Salary")], ncol = 1)
beta <- matrix(fit$coefficients, ncol = 1)
p <- length(beta)
X <- matrix(c(rep(1, n), mydata[,c("Algebra")],mydata[,c("Geometry")]), ncol = p)
C <- matrix(c(0, 1,1), ncol = 3)
d <- 0
side <- C %*% beta - d
D.SSE <- t(side) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% side
SSE <- t(y - X %*% beta) %*% (y - X %*% beta)
(F.stat <- (D.SSE/SSE) * (n - p)/(p-1))
pVal<-pf(F.stat, p-1, n-p, lower.tail = FALSE)
pVal
print(paste("F-статистика для математических наук: ", F.stat,
  ', p value = ', pVal, sep = ''))

fit<-lm(Salary ~ ., data = mydata[,c("Algebra","Geometry","Salary")])
summary(fit)


#F-статистика для естественных наук
fit <- lm(Salary ~ ., data = mydata[,c(nat_sciSubject,"Salary")])
n <- length(mydata[,c("Salary")])
y <- matrix(mydata[,c("Salary")], ncol = 1)
beta <- matrix(fit$coefficients, ncol = 1)
p <- length(beta)
X <- matrix(c(rep(1, n), mydata[,c("Geography")],mydata[,c("Biology")],
              mydata[,c("Physics")],mydata[,c("Chemistry")]), ncol = p)
C <- matrix(c(0, 1,1,1,1), ncol = p)
d <- 0
side <- C %*% beta - d
D.SSE <- t(side) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% side
SSE <- t(y - X %*% beta) %*% (y - X %*% beta)
(F.stat <- (D.SSE/SSE) * (n - p)/(p-1))
pVal<-pf(F.stat, p-1, n-p, lower.tail = FALSE)
pVal
print(paste("F-статистика для естественных наук: ", F.stat,
            ', p value = ', pVal, sep = ''))

fit<-lm(Salary ~ ., data = mydata[,c(nat_sciSubject,"Salary")])
summary(fit)

# 5 task
#Используйте логистическую регрессию, чтобы изучить как переменные,
#выбранные на втором шаге,связаны с получением зарплаты >50000.


myPrdict<-function(x,w)
{
  bb<-w[1]+x[,1]*w[2]+x[,2]*w[3]+x[,3]*w[5]+x[,4]*w[5]
  bb<-1/(1+exp(-bb))
  return(bb)
}


lg_df <- need_df4[-1]
lg_df
lg_df['Y'] <- (need_df4['Salary'] > 50000)
lg_df
logit <- glm(Y ~ ., data = lg_df, family = 'binomial')
myPar<-summary(logit)$coefficients[,1]
lg_df
#(Intercept) -4.095339 Chemistry    0.043336 Literature   0.019447  
#Geometry    -0.022803 English      0.012117
Y_hat<-myPrdict(lg_df[-5],myPar)
Y_hat
lg_df['Yhat'] <- (Y_hat > 0.5)

summ<-0
for(i in 1:length(lg_df[,1]))
{
  if(lg_df[i,'Yhat'] == lg_df[i,'Y'])
  {
    summ<-summ+1
  }
}
print(paste("Accuracy: ",round(summ/length(lg_df[,1])*100,2)," %",sep=""))





