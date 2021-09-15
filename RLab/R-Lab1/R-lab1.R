# Install dplyr package
install.packages("dplyr")   

# Load dplyr package    
library("dplyr")     

########
#Task1
########

#a)
#i
a <- matrix(c(1,2,3), nrow = 3)
b <- matrix(c(3, -5, 0), nrow = 3)

A <- matrix(c(3, 1.5, 1/8, 5, pi*-1, -6, -1, exp(2.5), 9), nrow = 3, ncol = 3)
B <- matrix(c(1,-2,3,-2,4,-6,3,-6,9), nrow = 3, ncol = 3)

#ii
# A %*% a
# B %*% b
# A^2
# A %*% B
# t(a) %*% b

#iii
# a*b, a+b, a- b, a/b



#iv)
BB <- rbind(colSums(B[1:2, 1:3]), B[3:3, 1:3])
BBQ <- cbind(rowSums(BB[1:2, 1:2]), BB[1:2, 3:3])
#b)

x <- c(5, 2, 6, 4, 1, 2, 2, 5, 4, 4, 6, 4, 2, 5, 5, 3, 6, 1, 4, 5)

efex <- cut(x, breaks = 3, labels = 1:3)

#Task2
#i, ii
v1 <- c(TRUE, TRUE, FALSE, TRUE, FALSE); v2 <- 1:6; v3 <- 5:10; v4 <- v1+0

#iii

q1 <- sum(v2 * v3)

listmult <- function(x, y){
  
  k <- 0
  
  for (i in 1:length(x)) {
    
    k <- k +  x[i]*y[i]
    
  }
  
  return(k)
  
}

example.function <- function(vec1, vec2){
  
 k <- 0  
 if(length(vec1) == length(vec2)){
   
   for (i in 1:length(vec2)) {
     
     k <- k +  (vec2[i]*vec1[i])^i
     
   }
   
 } else {
   
   len <- min(length(vec1), length(vec2))
   
   vec1 <- vec1[1:len]
   vec2 <- vec2[1:len]
   
   vec <- vec1 >= vec2
   
   k <- which(vec == TRUE)[1]
   
 }
 
 return(k)
  
}

#Task3

#a)
#i)
data.frame <- read.table("credits.wsv", header = TRUE)

#ii)
#show the table except first 8 columns data.frame[,-(1:8)]
#select the last 6 rows : tail(data.frame, n = 6)

#iii)
#mean : mean(data.frame$amount) 3271.248
#median : median(data.frame$amount) 2319.5

#b)
#the internal function sd returns corrected standard deviation
my.sd <- function(data, corrected = TRUE){

  n <- 0
  
  if(corrected){
    
    n <- length(data) - 1
    
  } else {
    
    n <- length(data)
    
  }
  
  return(sqrt(sum((data - mean(data))^2)/n))
  
}

#Task 4 
#a) rnorm(n = 10, 50, 100, mean = 5, sd = 4)

#i) n = 10
n10 <- rnorm(10, mean = 5, sd = 2)
#ii) n = 50
n50 <- rnorm(50, mean = 5, sd = 2)
#iii) n = 100
n100 <- rnorm(100, mean = 5, sd = 2)

#b)
#proportion lying between 3 and 7
#
length(which(between(n10, 3, 7)==TRUE))/10
length(which(between(n50, 3, 7)==TRUE))/50
length(which(between(n100, 3, 7)==TRUE))/100

#proportion lying between 5 and 9
length(which(between(n10, 5, 9)==TRUE))/10
length(which(between(n50, 5, 9)==TRUE))/50
length(which(between(n100, 5, 9)==TRUE))/100
#c)
quantile(n10, 0.3)
quantile(n10, 0.5)
quantile(n10, 0.75)
