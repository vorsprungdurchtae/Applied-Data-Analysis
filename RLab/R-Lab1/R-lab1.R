#Task 1
#1-(a)-i
a <- c(1, 2, 3)
b <- c(3, -5, 0)
A <- matrix(c(3, 1.5, 1/8, 5, -pi, -6, -1, exp(2.5), 9), nrow = 3, ncol = 3)
B <- matrix(c(1, -2, 3, -2, 4, -6, 3, -6, 9), nrow = 3, ncol = 3)

#1-(a)-ii
A %*% a
B %*% b
A %*% A
A %*% B
t(a) %*% b

#1-(a)-iii
#Just b-/+/ / /* a
plushat <- function(A, B) {

  if(length(A) != length(B)){
    
    print("if the length doesn't equal, the operation is impossible")
    
    stopifnot(length(A) == length(B))
  
    } else {
    
    return(A + B)
    
    }
  
}

minushat <- function(A, B) {
  
  if(length(A) != length(B)){
    
    print("if the length doesn't equal, the operation is impossible")
    
    stopifnot(length(A) == length(B))
    
  } else {
    
    return(A - B)
    
  }
  
}

multihat <- function(A, B) {
  
  if(dim(A)[0] != dim(B)[0] && dim(A)[1] != dim(B)[1]){
    
    print("if the length doesn't equal, the operation is impossible")
    
    stopifnot(dim(A) != dim(B))
    
  } else {
    
    for (x in 1:length(A)) {
      
      print(A[x]*B[x])
      
    }
    
    
  }
  
}

colonhat <- function(A, B) {
  
  if(dim(A)[0] != dim(B)[0] && dim(A)[1] != dim(B)[1]){
    
    print("if the length doesn't equal, the operation is impossible")
    
    stopifnot(dim(A) != dim(B))
    
  } else {
    
    for (x in 1:length(A)) {
      
      print(A[x]:B[x])
      
    }
    
    
  }
  
}


#1-(a)-iiii  B'

new_B.1 <- rbind(sum(B[1,], B[2,]), B[3,])

new_B <- cbind(sum(new_B.1[,1], new_B.1[,2]), new_B.1[,3])

#1-(b)
x <- c(5L, 
       2L, 
       6L, 
       4L, 
       1L, 
       2L, 
       2L, 
       5L, 
       4L, 
       4L, 
       6L, 
       4L, 
       2L, 
       4L, 
       5L, 
       5L, 
       3L, 
       6L, 
       1L, 
       4L, 
       5L)

(y <- cut(x, breaks = 3, labels = c(1,2,3), include.lowest = TRUE, right = TRUE))
y <- as.numeric(y)

#(y <- ceiling(x/2))

#Task 2
#2-(a)#
v1 <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
v2 <- 1:6
v3 <- 5:10
v1; v2; v3

#2-(b)
#the R interprets True = 1, False = 0
print(sum(v1))
print(prod(v1))
v4 <- v1*1

#2-(c)
#i) with loop

withloop <- function(A, B) {
  
  if(length(A) != length(B)){
    
    print("if the length doesn't equal, the operation is impossible")
    
    stopifnot(length(A) != length(B))
    
  } else {
    
    storage <- numeric(length(A))
    
    for (i in 1:length(A)) {
      
      storage[i]<-(A[i]*B[i])^i
      
    }
    
  return(sum(storage))
  
  }
  
}

#ii) without loop
withoutloop <- function(A, B) {
  
  if(length(A) != length(B)){
    
    print("if the length doesn't equal, the operation is impossible")
    
    stopifnot(length(A) != length(B))
    
  } else {
    
   result <- A*B
   k <- 1:length(A)
   result <- result^k
      
  }
    
  return(sum(result))
  
}

#2-d)-i

searchloop <- function(v2, v4) {
  
  i <- 1
  res <- 0
  
  while(i < length(v2)) {
    
    if (v2[i] > max(v4)){
      
      res <- v2[i]
      break
      
    } 
    i <- i + 1
  }
  
  if(res > 0){
  
    return(res)
    
  } else {
    
    print("no element is lager than v4")
    return(NULL)
    
  }
  
}

#which(v2[-6] > v4)[1]

#2-d)-ii

searchnoloop <- function(vec1, vec2) {
  
  l <- (vec1 - max(vec2)) *TRUE
  return(match(TRUE, l))
  
}

#2-e)

example.function <- function(vec1, vec2) {
  
  if(length(vec1) == length(vec2)){
    
    return(withoutloop(vec1, vec2))
    
  } else {
    
    shorter <- if(length(vec1) >= length(vec2)) length(vec2) else length(vec1)
    
    i <- searchnoloop(vec1[1:shorter], vec2[1:shorter])
    
  }
  
  return(i)
  
}

#Task 3
#3-a)-i
dataset <- read.csv(file = "/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab1/credits.wsv",
                    sep=" ")

#3-a)-ii
#print the column amount
#print(dataset[, "amount"])
#credits.data$amount

#the type of column "amount"
#print(class(dataset[, "amount"]))

#second cloumn
#print(dataset[,2])

#second row
#print(dataset[2,])

#every row but apart from the first eight rows
#print(dataset[-(1:8),])

#every row but apart from the first eight columns
#print(dataset[,-(1:8)]

#first six rows
#print(head(dataset, n = 6))

#last six rows
#print(tail(dataset, n = 6))

#3-a)-iii mean and median of the variable amount
# and print the summary of amount to the console

#print(mean(dataset[, "amount"]))
#print(median(dataset[, "amount"]))

#summary(dataset[, "amount"])

#3-b)-i

my.sd <- function(data, corrected = TRUE) {
  
  my.mean <- mean(data)
  new_data_sum <- sum((data - my.mean)^2)
  denom <- length(data) - corrected

  return(sqrt(new_data_sum/denom))

}

#3-b)-ii
#sd uses corrected=TRUE
#sd(dataset[, "amount"])
#my.sd(dataset[, "amount"])
#my.sd(dataset[, "amount"], corrected = FALSE)

#4-a
n10 <- rnorm(10, 5, 2)
n50 <- rnorm(50, 5, 2)
n100 <- rnorm(100, 5, 2)

#4-b

h1 = mean(3 <= n10 & n10 <= 7)
h2 = mean(5 <= n10 & n10 <= 9)

p = pnorm(c(7,9), mean = 5, sd = 2) - pnorm(c(3,5), mean = 5, sd = 2)

#4-c

#quantile(n10, probs = 0.3)
#quantile(n50, probs = 0.3)
#quantile(n100, probs = 0.3)

#quantile(n10, probs = 0.5)
#quantile(n50, probs = 0.5)
#quantile(n100, probs = 0.5)


#quantile(n10, probs = 0.75)
#quantile(n50, probs = 0.75)
#quantile(n100, probs = 0.75)

#qnorm(0.3, 5, 2)
#qnorm(0.5, 5, 2)
#qnorm(0.75, 5, 2)
