######################
#########TASK1########
######################
#a)
#i)
a <- c(1,2,3)
b <- c(3, -5, 0)

A <- array(c(3, 1.5, 1/8, 5, -pi, -6, -1, exp(2.5), 9), dim= c(3,3))

B <- array(c(1, -2, 3, -2, 4, -6, 3, -6, 9), dim= c(3,3))

#ii) A.a, B.b, A^2, A.B, a.T.b

#A.a 
A %*% a
#B.b
B %*% b
#A^2
A %*% A
#A.B
A %*% B
#a.T.B
t(a) %*% b

#iii) Component-wise operation of +, -, *, /
# B^+A
B + A
#B^-A
B - A
#B^*A
c(B)*c(A)
#B^:A?
c(B):c(A)

#iv) B' R2X2
bb <- t(array(c(B[1,] + B[2,], B[3,]), dim = c(3,2)))
result = array(c(bb[,1] + bb[,2], bb[,3]), dim = c(2,2))

#b)
x <- c(5, 2, 6, 4, 1, 2, 2, 5, 4, 4, 6, 4, 2, 5, 5, 3, 6, 1, 4, 5)

interval = (0:3)
label = (1:3)

cuttingx <- cut(x/2, breaks = interval, labels = label)

######################
#########TASK2########
######################
#a)
v1 <- c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE)
v2 <- 1:6
v3 <- 5:10
v4 <- v1*1

#c)
sum((v2*v3)^c(1:length(v2)))

ptw_mult <- function(a, b) {
  
  result = rep(0, length(a))
  
  for(i in 1:length(a)) {
    
    result[i] <- (a[i]*b[i])^i
    
  }
  
  return(sum(result))
  
}

#d)
which(v2 > v4)[1]

mywhich <- function(a1, a2){
  
  stopifnot(length(a1) == length(a2))
  
  index <- 0
  
  for(i in 1: length(a1)){
    
    if(a1[i] > a2[i]){
      
      index <- i
      break
      
    }
    
  return(index)
    
  }
  
}

#e)

example.function <- function(vec1, vec2){
  
  result <- 0
  
  if(length(vec1) == length(vec2)){
    
    result <- sum((v1*v2)^c(1:length(v2)))
    
  } else {
    
    min_len <- min(length(vec1), length(vec3))
    
    new_vec1 <- vec1[1 : min_len]
    new_vec2 <- vec2[1 : min_len]
    
    result <- which(new_vec1 > new_vec2)[1]
    
  }
  
  return(result)
  
}

######################
#########TASK3########
######################
#a)
#i)
#credits <- read.csv("credits.csv")
credits <- read.table("credits.csv", header = TRUE, sep = ",")
#ii)
#determine the type of the column "amount"
#sapply(credits["amount"], typeof)
#print the second row
# credits[2,]
#second column
# credits[,2]
#apart from the first eight rows
# credits[-8,]
# 
#iii)
amount <- credits[,7]
mean_amount <- sum(amount)/length(amount)
median_amount <- median(amount)

#b)

my.sd <- function(data, corrected = TRUE){
  
  result <- 0
  
  if(corrected){
    
    result = sqrt(sum((data - mean(data))^2)/(length(data)-1))
    
  } else {
    
    result = sqrt(sum((data - mean(data))^2)/(length(data)))
    
  }
  
  return(result)
  
}

######################
#########TASK4########
######################
#a)
r10 <- rnorm(10,5,2)
r50 <- rnorm(50,5,2)
r100 <- rnorm(100,5,2)

#b)
#pnorm(7, 5, 2) - pnorm(3, 5, 2)
#pnorm(7, 5, 2) - pnorm(3, 5, 2)
#[1] 0.6826895

#> pnorm(9, 5, 2) - pnorm(5, 5, 2)
#[1] 0.4772499

#qnorm(0.85,mean=70,sd=3)

mean(r10)
sd(r10)
qnorm(0.3, 5.5899, 2.755)