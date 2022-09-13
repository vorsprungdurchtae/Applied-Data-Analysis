library(ggplot2)
library(mvtnorm)
library(MASS)
library(dplyr)
library(tidyr)
library(gridExtra)
library(car)

set.seed(518)

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


###################
#######TASK5#######
###################
#a)
#draw random sample for each 30, 100, 300
norm_sample_30 <- rnorm(30, 5, 2)
norm_sample_100 <- rnorm(100, 5, 2)
norm_sample_300 <- rnorm(300, 5, 2)

#vizualize the density of each point for the given random samples
plot(density(norm_sample_30))
plot(density(norm_sample_100))
plot(density(norm_sample_300))

#b)
#draw the histogram of random sample 300 with probability
#otherwise it shows the frequency
# frequency always > 1, which is way more great than the density
# because density is always <= 1
hist(norm_sample_300, prob = TRUE)
# lines is the function to add lines of the given data
# to the plot
lines(density(norm_sample_300), lwd = 2, col = "red")

###################
#######TASK6#######
###################

#a) Draw random sample of size n = 100 and N(0, I4) distribution
sigma_task6 <- diag(4)
norm_sample_100_I4 <- rmvnorm(100, 
                              mean = rep(0, nrow(sigma_task6)),
                              sigma = sigma_task6)

#b) Initialize a vector μ = (1, 0, 2, −1)′ and matrices
# pseudo mean mu = (1, 0, 2, -1)
pseudo_mean_task6 <- c(1, 0, 2, -1)

# pseudo matrix sigma 1
pseudo_matrix_task6_1 <- matrix(c(4, 2, 2, 3,
                                  2, 3, 2, 1,
                                  2, 2, 5, 2,
                                  3, 1, 2, 3), nrow = 4, ncol = 4)
# pseudo matrix sigma 2
pseudo_matrix_task6_2 <- matrix(c(4.5,  4.75, 2,    2.25,
                                  4.75, 5.25, 2.75, 3.25,
                                  2,    2.75, 2.75, 3.5,
                                  2.25, 3.25, 3.5,  4.5), nrow = 4, ncol = 4)


# c)
# SVD of pseudo matrix sigma 1
# svd(x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE)

# x target vector or matrix whose SVD decomposition is to be computed..
# nu the number of left singular vectors to be computed. This must between 0 and n = nrow(x).
# nv the number of right singular vectors to be computed. This must be between 0 and p = ncol(x).
SVD_pseudo_matrix_task6_1 <- svd(pseudo_matrix_task6_1, 
                                 nu = nrow(pseudo_matrix_task6_1),
                                 nv = ncol(pseudo_matrix_task6_1))
# to see the u, d, and v 
# SVD_pseudo_matrix_task6_1$u
# SVD_pseudo_matrix_task6_1$d
# SVD_pseudo_matrix_task6_1$v

SVD_pseudo_matrix_task6_1$d <- replace(SVD_pseudo_matrix_task6_1$d, 
                                       SVD_pseudo_matrix_task6_1$d < .Machine$double.eps,
                                       0)

#Now! Transform the sample from a to the sample N(pseudo_mean_task6, 
#                                                pseudo_matrix_task6_1)

SVD_pseudo_matrix_task6_1_sqrt <- SVD_pseudo_matrix_task6_1$u %*% 
  diag(sqrt(SVD_pseudo_matrix_task6_1$d))  %*% 
  t(SVD_pseudo_matrix_task6_1$v)

# before add the mean, it should be duplicated
# i.e. it is just R 4X1, the dimension does not match each other 
new_sample1_task6 <- matrix(pseudo_mean_task6, nrow = 4, ncol = 100) + SVD_pseudo_matrix_task6_1_sqrt %*% t(norm_sample_100_I4)



# SVD of pseudo matrix sigma 2
SVD_pseudo_matrix_task6_2 <- svd(pseudo_matrix_task6_2, 
                                 nu = nrow(pseudo_matrix_task6_2),
                                 nv = ncol(pseudo_matrix_task6_2))

SVD_pseudo_matrix_task6_2$d <- replace(SVD_pseudo_matrix_task6_2$d, 
                                       SVD_pseudo_matrix_task6_2$d < .Machine$double.eps,
                                       0)


SVD_pseudo_matrix_task6_2_sqrt <- SVD_pseudo_matrix_task6_2$u %*% 
  diag(sqrt(SVD_pseudo_matrix_task6_2$d))  %*% 
  t(SVD_pseudo_matrix_task6_2$v)

# before add the mean, it should be duplicated
# i.e. it is just R 4X1, the dimension does not match each other 
new_sample2_task6 <- matrix(pseudo_mean_task6, nrow = 4, ncol = 100) + SVD_pseudo_matrix_task6_2_sqrt %*% t(norm_sample_100_I4)

# d)
#cex : scale of the plot
#pch : shape of point, triangle = 2, inverse triange = 6, ...
plot(t(new_sample1_task6), pch = 20, cex = 1.5, col = "#69b3a2")

plot(t(new_sample2_task6), pch = 20, cex = 1.5, col = "#69b3a2")

plot(norm_sample_100_I4, pch = 20, cex = 1.5, col = "#69b3a2")

# e) 
# first the second question! yes!
# regular matrix, det(A) != 0
# if the matrix is regular, A^+ = A^-1
SVD_pseudo_matrix_task6_1_eigenvalue <- diag(SVD_pseudo_matrix_task6_1$d)

ginv_pseudo_matrix_task6_1 <- SVD_pseudo_matrix_task6_1$u %*% 
  ginv(SVD_pseudo_matrix_task6_1_eigenvalue) %*% 
  t(SVD_pseudo_matrix_task6_1$v)

inv_pseudo_matrix_task6_1 <- solve(pseudo_matrix_task6_1)


SVD_pseudo_matrix_task6_2_eigenvalue <- diag(SVD_pseudo_matrix_task6_2$d)

ginv_pseudo_matrix_task6_2 <- SVD_pseudo_matrix_task6_2$u %*% 
  ginv(SVD_pseudo_matrix_task6_2_eigenvalue) %*% 
  t(SVD_pseudo_matrix_task6_2$v)

#inv_pseudo_matrix_task6_2 <- solve(pseudo_matrix_task6_2)

###################
#######TASK7#######
###################
# # a) import the both csv data 
# data.frame.survey1a <- read.table("survey1a.csv", header = TRUE, sep = ";")
# 
# data.frame.survey1b <- read.table("survey1b.csv", header = TRUE, sep = ";")
# 
# # b) transform the measured dimensions and the mean score to tpye numeric
# # sapply() function takes list, vector or data frame as input and gives output in vector or matrix.
# data.frame.survey1a[,5:ncol(data.frame.survey1a)] <- sapply(sapply(data.frame.survey1a[,5:ncol(data.frame.survey1a)], as.character), as.numeric)
# data.frame.survey1b[,5:ncol(data.frame.survey1b)] <- sapply(sapply(data.frame.survey1b[,5:ncol(data.frame.survey1b)], as.character), as.numeric)
# 
# #idx = (regexpr("Dim, names(data.survey.a)) > -1|names(data.survez.a) == "MeanScore")
# 
# # c) merge two dataframes
# 
# # merge the two dataframes
# data.frame <- rbind(data.frame.survey1a, data.frame.survey1b)
# 
# #missing.values = which(is.na(data.survey1b), arr.ind = TRUE)
# 
# # fill the NA values of each columne
# # bad_colnames <- names(which(colSums(is.na(data.frame)) > 0))
# # fill(data.frame, .direction = "downup")
# # %>% imports dplyr
# data.frame <- data.frame %>% fill(names(data.frame), .direction = "downup")
# 
# # filter the duplicated rows 
# data.frame <- data.frame %>% filter(duplicated(data.frame))
# 
# # d) scatterplot X : Age, Y : DimSchool diff: SEX 
# 
# # dataframe only man
# df_m <- data.frame[data.frame$Sex == 'm', ]
# 
# # dataframe only girls
# df_f <- data.frame[data.frame$Sex == 'f', ]
# 
# #single plotting
# plot(df_m$Age, df_m$DimSchool, pch = 20, col = "blue")
# plot(df_f$Age, df_f$DimSchool, pch = 20, col = "red")
# 
# #base_plot <- base_plot +
# #aes(color = Home.Value)
# age_dimschool_plot <- ggplot(mapping = aes(x = data.frame$Age,
#                                            y = data.frame$DimSchool))
# age_dimschool_plot <- age_dimschool_plot + aes(color = data.frame$Sex) + geom_point()
# 
# age_dimfriends_box_plot <- ggplot(mapping = aes(x = data.frame$Age,
#                                                 y = data.frame$DimFriends))
# 
# age_dimfriends_box_plot <- age_dimfriends_box_plot + aes(color = data.frame$Sex) + geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)
# 
# # e) save the data.frame to .RData
# write.csv(data.frame, file = "df_task7", row.names = FALSE)

# a) read the dataframe by using read.csv
# recommended to use read.csv rather than read.table
data.frame.survey1a <- read.csv2("Survey1a.csv", stringsAsFactors = TRUE)
data.frame.survey1b <- read.csv2("Survey1b.csv", stringsAsFactors = TRUE)

# b)
# extract the column index with column name of "Dim" and "MeanScore"
idx <- (regexpr("Dim", names(data.frame.survey1a)) > -1)|names(data.frame.survey1a) == "MeanScore"

data.frame.survey1a[, idx] <- sapply(sapply(data.frame.survey1a[, idx], as.character), as.numeric)

data.frame.survey1b[, idx] <- sapply(sapply(data.frame.survey1b[, idx], as.character), as.numeric)

# c) select missing values

missing.values <- which(is.na(data.frame.survey1b), arr.ind = TRUE)

for(row in missing.values[,1]){
  
  #assign data.frame.survey.1b with the corresponding values 
  #from data.frame.survey.1a
  
  data.frame.survey1b[row,]=data.frame.survey1a[data.frame.survey1a$Person==data.frame.survey1b$Person[row] &data.frame.survey1a$Date == as.character(data.frame.survey1b$Date[row]), ]
}

# concatenate vertically
data.frame.survey <- rbind(data.frame.survey1a, data.frame.survey1b)

# delete duplicated rows
data.frame.survey <- data.frame.survey[!duplicated(data.frame.survey), ]

save(data.frame.survey, file = "df_task7_survey1.RData")

write.csv(data.frame.survey, file = "df_task7_survey", row.names = FALSE)

###################
#######TASK8#######
###################

# a) load credits.wsv
data.frame.credits <- read.table("credits.wsv", header = TRUE, sep = " ")

# b) switch the binary variable gastarb 2 from to 1, 1 to 2 
data.frame.credits$gastarb <- 3 - data.frame.credits$gastarb
#also credits$gastarb%%2 + 1

# c) using cut
# time (0, 6] 10,  (6, 12] 9,  (12, 18] 8, ... ,  (58, inf] 0,   
# into dtime column

#x <- c(5, 2, 6, 4, 1, 2, 2, 5, 4, 4, 6, 4, 2, 5, 5, 3, 6, 1, 4, 5)

#interval = (0:3)
#label = (1:3)

#cuttingx <- cut(x/2, breaks = interval, labels = label)

time_score_cuts = c(0, 6,  12, 18,  24, 30,   36, 42, 48, 54, Inf)
time_score_label = c (10, 9,  8,  7,   6,   5,   4,  3,  2,  1)
#as.numeric(as.factor)
time_score <- c(cut(data.frame.credits$time, breaks = time_score_cuts, labels = time_score_label))

amount_score_cuts = c(0, 500, 1000, 1500, 2500, 5000, 7500, 10000, 150000, 20000, Inf)
amount_score_label = c (10, 9,  8,  7,   6,   5,   4,  3,  2,  1)
amount_score <- c(cut(data.frame.credits$amount, breaks = amount_score_cuts, labels = amount_score_label))

age_score_cuts = c(0, 25, 39, 59, 64, Inf)
age_score_label = c(1,2,3,4,5)
age_score <- c(cut(data.frame.credits$age, breaks = age_score_cuts, labels = age_score_label))

data.frame.credits$dtime <- time_score
data.frame.credits$damount <- amount_score
data.frame.credits$dage <- age_score

to_sum_columns <- c("account", 
                    "dtime", 
                    "behavior",
                    "usage",
                    "damount",
                    "savings",
                    "employment",
                    "rate",
                    "famgen",
                    "guaran",
                    "residence",
                    "finance",
                    "dage",
                    "furthcred",
                    "home",
                    "prevcred",
                    "job",
                    "pers",
                    "phone",
                    "gastarb")


data.frame.credits$simple.score = rowSums(data.frame.credits[,to_sum_columns])

#d)

mean(data.frame.credits$simple.score[data.frame.credits$repayment == 0])
mean(data.frame.credits$simple.score[data.frame.credits$repayment == 1])
# e) save the file into csv
write.csv(data.frame.credits, file = "df_task8", row.names = FALSE)


###################
#######TASK9#######
###################

#a) draw random sample size m = 30, n = 12, p = 0.7
# meaning:
# we have 12 trial with success probability 0.7
# if the result is 4, we got 4 success within 12 trials
binom_sample <- rbinom(30, 12, 0.7)

#b) bar plot and probability mass function of binom_sample

binom_bar <- barplot(table(binom_sample),
        xlab = "Values",
        ylab = "Frequencies")
        
#By using the sapply compute the pmf of the given sample 
pmf_mat <- dbinom(sort(unique(binom_sample)), size = 12, prob = 0.7)

lines(x = binom_bar, y = pmf_mat * 20)

#c) calculate mean and variance of the sample and 
#calculate the the Tm,n,p

# mean of sample

mean_binom_sample <- mean(binom_sample)
variance_binom_sample <- var(binom_sample)

title(main = paste("mean:", mean_binom_sample, "var:", variance_binom_sample))

T_m_n_p <- sqrt(m)*(mean_binom_sample - 12*0.7)/sqrt(12*0.7*0.3)

new_T_m_n_p <- function(m, n, p) {
  
  return(sqrt(m)*(mean(rbinom(m, n, p)) - n*p)/sqrt(n*p*(1-p)))
  
}

#replicates_5 <- replicate(10000, new_T_m_n_p(5, 12, 0.7))

####################
#######TASK10#######
####################
#a) read the data and change the type of batch
#read.csv 
data.frame.solar <- read.csv2("Solar.csv", header = TRUE, sep = ";")

#b) scatter plot matrix of the attributes Pmax, Imax, Umax, and Uoc, 
# differentiate the points by batch using colors

#data.survey.a[, idx] <- sapply(sapply(data.survey.a[, idx], as.character), as.numeric)
#data.survey.b[, idx] <- sapply(sapply(data.survey.b[, idx], as.character), as.numeric)
#data.frame.solar&pmax()
#transfrom the solar data set using transform
data.frame.solar <- transform(data.frame.solar, 
          Pmax = as.numeric(sub(",", "",Pmax)), 
          Imax = as.numeric(sub(",", "",Imax)),
          Umax = as.numeric(sub(",", "",Umax)),
          Isc = as.numeric(sub(",", "",Isc)),
          Uoc = as.numeric(sub(",", "",Uoc)), 
          batch = as.factor(batch))

# fill NA in Isc column
# colSums(is.na(data.frame.solar)) to find the columns with NA
data.frame.solar$Isc[is.na(data.frame.solar$Isc)] <- median(data.frame.solar$Isc, na.rm = TRUE)

#b) create box plot
to_scatter_plot_list <- c("Pmax", 
                          "Imax", 
                          "Umax",
                          "Isc",
                          "Uoc")

batch.colors <- c("#69b3a2", "blue", "hotpink", "green3")

pairs(data.frame.solar[,to_scatter_plot_list], 
      col = batch.colors[data.frame.solar$batch],
      pch = 19,
      oma=c(4,4,6,12))

par(xpd = TRUE)

legend(0.85,
       0.7,
       legend = as.vector(unique((data.frame.solar$batch))),
       fill = batch.colors)
       
#c) create Box-plots for Uoc each batch in one figure

df.uoc_batch <- data.frame.solar[c("Uoc", "batch")]

uoc_batch_box_plot <- ggplot(df.uoc_batch, aes(x = batch, y = Uoc, fill=batch)) + geom_boxplot()

#d)
#read data again with missing values
new_solar <- read.table("solar.csv", header = TRUE, sep = ";")
#transform the type of each column
new_solar <- transform(new_solar, 
                              Pmax = as.numeric(sub(",", "",Pmax)), 
                              Imax = as.numeric(sub(",", "",Imax)),
                              Umax = as.numeric(sub(",", "",Umax)),
                              Isc = as.numeric(sub(",", "",Isc)),
                              Uoc = as.numeric(sub(",", "",Uoc)), 
                              batch = as.factor(batch))


Isc_without_na <- na.omit(new_solar)$Isc

Pmax_without_na <- na.omit(new_solar)$Pmax

Pmax_predictor <- cbind(rep(1, length(Pmax_without_na)), Pmax_without_na)
#    [,1]
#[1,] 3.305636e-07
#[2,] 5.863115e-02
# d.i)
parameter_vector <- ginv(t(Pmax_predictor) %*% Pmax_predictor) %*% t(Pmax_predictor) %*% Isc_without_na
# d.ii)
fit.isc.pmax <- lm(Isc_without_na ~ Pmax_without_na)
# intercept 9.671e+03 (9671)
# coefficient 4.104e-03 (0.0004104)

predicted_Isc_Pmax <- predict.lm(fit.isc.pmax, newdata = list(new_solar$Pmax))

batch1_idx <- which(new_solar$batch == 1)
batch4_idx <- which(new_solar$batch == 4)

Pmax_batch1 <- new_solar[batch1_idx,]$Pmax
Isc_predicted_batch1 <- predicted_Isc_Pmax[batch1_idx]

Pmax_batch4 <- new_solar[batch4_idx,]$Pmax
Isc_predicted_batch4 <- predicted_Isc_Pmax[batch4_idx]

#scatter plot of X Axis : Pmax, Y Axis : Isc colored by batch values 
ggplot(new_solar, aes(Pmax, Isc, color = batch)) +
  geom_point()
lines(Pmax_batch1, Isc_predicted_batch1)

####################
#######TASK11#######
####################

#a)
data.frame.rent <- read.table("rent.csv", header = TRUE, sep = ";")
data.frame.rent <- transform(data.frame.rent, 
                       rent = as.numeric(sub(",", "",rent)), 
                       rent.sqm = as.numeric(sub(",", "",rent.sqm)))
#scatter plot

#get the parameter vector of rent.sqm  ~ space
fit.space_sqm <- lm(data.frame.rent$rent.sqm ~ data.frame.rent$space)

#apply predict
predicted_rent <- predict.lm(fit.space_sqm, list(data.frame.rent$space))


#ggplot_space_rent <- ggplot(data.frame.rent, 
#                            aes(x = data.frame.rent$space,
#                                y = data.frame.rent$rent.sqm,
#                                color = "space")) + 
#                                geom_point() + 

plot_space_rent <- plot(data.frame.rent$space, data.frame.rent$rent.sqm, pch = 20, col = "blue")

lines(data.frame.rent$space, predicted_rent, col = "#69b3a2")
      
reversed_space_plot <- plot(1/(data.frame.rent$space), data.frame.rent$rent.sqm, pch = 20, col = "red")

#get the parameter vector of rent.sqm  ~ space
fit.reversed_space_sqm <- lm(data.frame.rent$rent.sqm ~ I(1/data.frame.rent$space))

#apply predict
reversed_predicted_rent <- predict.lm(fit.reversed_space_sqm, list(1/data.frame.rent$space))

lines(1/data.frame.rent$space, reversed_predicted_rent, col = "paleturquoise")

# not reserved one 

#c) yes!

####################
#######TASK12#######
####################

#a) read the car2.dat cars2.dat

data.frame.cars2 <- read.table("cars2.dat", header = TRUE)

#b)
cars_scatter_plot <- plot(data.frame.cars2$speed, data.frame.cars2$dist, pch = 19, col = "blue")

#c)
fit.speed_speedqr_dist <- lm(data.frame.cars2$dist ~ data.frame.cars2$speed + I(data.frame.cars2$speed^2))

# numerically stable
fit.speed_speedqr_dist <- lm(data.frame.cars2$dist ~ poly(data.frame.cars2$speed, 2))

#predicted_dist_speed_speedqr <- predict(fit.speed_speedqr_dist, )

summary.cars2 <- summary(fit.speed_speedqr_dist)

predicted_speed_speedqr_dist <- predict(fit.speed_speedqr_dist, list(data.frame.cars2$speed))

speed_lines <- lines(data.frame.cars2$speed, predicted_speed_speedqr_dist, col = "#69b3a2")

#d) c = 0 => the second polynomial to be zero


####################
#######TASK13#######
####################

beta_init <- c(1, 0.1, 5*10^-4, 5*10^-7, 5*10^-11, 5*10^-13)

alpha_init <- 10

set.seed(2020)

#initialize two vectors with length 100
vec.delta.simple <- rep(0, 100)
vec.delta.correct <- rep(0, 100)

for(i in 1:100) {
  
  sample_task13 <- runif(25, 0, 100)
  ones_25 <- rep(1, 25)
  exp.simple <- cbind(ones_25, 
                      sample_task13) %*% beta_init[1:2]
  exp.correct <- cbind(ones_25, 
                       sample_task13, 
                       sample_task13^2,
                       sample_task13^3,
                       sample_task13^4,
                       sample_task13^5) %*% beta_init
  
  observation.simple <- mvrnorm(1, exp.simple, 100 * diag(25))
  
  # for correct
  observation.correct <- mvrnorm(1, exp.correct, 100*diag(25))
  
  # fit the linear model simple
  fit.simple <- lm(observation.simple ~ sample_task13)
  
  # fit the correct model 
  fit.correct <- lm(observation.correct ~ poly(sample_task13 ,degree = 5))
  
  # achieve the predicted values of simple matrix
  
  predicted_simple_sample_task13 <- predict(fit.simple)
  
  # plot the simple
  plot_simple_task_13 <- ggplot(mapping = aes(x = sample_task13,
                                              y = observation.simple)) + geom_point() + geom_abline((aes(intercept = fit.simple$coefficients[1], slope = fit.simple$coefficients[2])), col = "#69b3a2")
  
  #plot the multinomial
  # get the prediction
  predicted_correct_sample_task13 <- predict(fit.correct)
  # plot the sample and predicted values
  plot(sample_task13, predicted_correct_sample_task13)
  # sort the sample and achieve the index
  ix <- sort(sample_task13, index.return = T)$ix
  # draw the line
  lines(sample_task13[ix], predicted_correct_sample_task13[ix], col = "#69b3a2")
  
  # now get the absolute value of the residualss
  
  vec.delta.simple[i] <- mean(sum(abs(predicted_simple_sample_task13 - exp.simple)))
  
  vec.delta.correct[i] <- mean(sum(abs(predicted_correct_sample_task13 - exp.correct)))
  
}

#get the 25 sample from [0, 100]
sample_task13 <- runif(25, 0, 100)

ones_25 <- rep(1, 25)

# extract the first two and multiply with the initial beta to 
# achieve the mean
exp.simple <- cbind(ones_25, 
                              sample_task13) %*% beta_init[1:2]

# same, but for the correct matrix
exp.correct <- cbind(ones_25, 
                              sample_task13, 
                              sample_task13^2,
                              sample_task13^3,
                              sample_task13^4,
                              sample_task13^5) %*% beta_init

# now extract the sample by using multivariate normal sample
# for simple
observation.simple <- mvrnorm(1, exp.simple, 100 * diag(25))

# for correct
observation.correct <- mvrnorm(1, exp.correct, 100*diag(25))

# fit the linear model simple
fit.simple <- lm(observation.simple ~ sample_task13)

# fit the correct model 
fit.correct <- lm(observation.correct ~ poly(sample_task13 ,degree = 5))

# achieve the predicted values of simple matrix

predicted_simple_sample_task13 <- predict(fit.simple)

# plot the simple
plot_simple_task_13 <- ggplot(mapping = aes(x = sample_task13,
                                           y = observation.simple)) + geom_point() + geom_abline((aes(intercept = fit.simple$coefficients[1], slope = fit.simple$coefficients[2])), col = "#69b3a2")


#plot the multinomial
# get the prediction
predicted_correct_sample_task13 <- predict(fit.correct)
# plot the sample and predicted values
plot(sample_task13, predicted_correct_sample_task13)
# sort the sample and achieve the index
ix <- sort(sample_task13, index.return = T)$ix
# draw the line
lines(sample_task13[ix], predicted_correct_sample_task13[ix], col = "#69b3a2")

# now get the absolute value of the residualss

residual_simple <- mean(sum(abs(predicted_simple_sample_task13 - exp.simple)))

residual_correct <- mean(sum(abs(predicted_correct_sample_task13 - exp.correct)))


####################
#######TASK14#######
####################

# a) load the survey data
data.frame.survey.task14 <- read.csv2("df_task7_survey", stringsAsFactors = TRUE, header = TRUE, sep = ",")

# b) create the regression model
# DimSelf = d + a*DimEmotion + b*DimBody

# convert the column type to apply the lm
data.frame.survey.task14$DimSelf <- sapply(data.frame.survey.task14$DimSelf, as.numeric)
data.frame.survey.task14$DimEmotion <- sapply(data.frame.survey.task14$DimEmotion, as.numeric)
data.frame.survey.task14$DimBody <- sapply(data.frame.survey.task14$DimBody, as.numeric)

DimSelf.lm <- lm(DimSelf ~ DimEmotion + DimBody, data = data.frame.survey.task14)


# c)-i get residuals
#predict the values
predict.DimSelf <- predict(DimSelf.lm)

# get the residuals
residuals.DimSelf <- data.frame.survey.task14$DimSelf - predict.DimSelf

# plot residuals (Y axis) vs predicted values (X axis)
plot(predict.DimSelf, residuals.DimSelf, col = "#69b3a2", pch = 20)
# 
# ix.Dimself <- sort(predict.DimSelf, index.return = T)$ix
# # draw the line
# lines(predict.DimSelf[ix], residuals.DimSelf[ix], col = "red")

# c)-ii get standardized residuals
# get the standardized residuals between DimSelf and predicted DimSelf
standard_res.DimSelf <- rstandard(DimSelf.lm)

# plot the standardized residuals vs fitted values
plot(predict.DimSelf, stadard_res.DimSelf, col = "#69b3a2", pch = 20)

# c)-iii 
#Quantiles of the standardized residuals vs expected quantiles of the 
#standard normal distribution

# get the quantiles from the standardized residuals
quantiles.std.residuals.Dimself <- quantile(standard_res.DimSelf)

# c)-iv get the leverage
# sort this ascending order, get the index, and plot them
hats <- as.data.frame(hatvalues(DimSelf.lm))

plot(hatvalues(DimSelf.lm), type = 'h')
# 
# ix <- sort(sample_task13, index.return = T)$ix
# # draw the line
# lines(sample_task13[ix], predicted_correct_sample_task13[ix], col = "#69b3a2")
# 
# c)-v cook's distance

plot(DimSelf.lm, 4)

# c) (ii) test on alpha = 0.5, evidence against the assumption of normally distributed residuals
# cannot assume normality
# data:  data.frame.survey.task14$DimSelf
# W = 0.95616, p-value = 0.006912
DimSelf.shapiro.test <- shapiro.test(data.frame.survey.task14$DimSelf)



####################
#######TASK15#######
####################

# load the race data
data.table.races <- read.table("Races.dat", header = TRUE)

# fit the linear model
fit.women.record.climb.distance <- lm(data.table.races$timeW ~ data.table.races$distance + data.table.races$climb)
# achieve the predictions
predict.women.record.distance.climb <- predict(fit.women.record)
# 
# Call:
#   lm(formula = data.table.races$timeW ~ data.table.races$distance + 
#        data.table.races$climb)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -37.209  -8.637   0.235   8.504  33.901 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               -14.5997     3.4680   -4.21 8.02e-05 ***
#   data.table.races$distance   5.0362     0.1683   29.92  < 2e-16 ***
#   data.table.races$climb     35.5610     3.7002    9.61 4.22e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 13.96 on 65 degrees of freedom
# Multiple R-squared:  0.9641,	Adjusted R-squared:  0.963 
# F-statistic: 872.6 on 2 and 65 DF,  p-value: < 2.2e-16

# fit the linear model just by using distance
# fit the linear model
fit.women.record.distance <- lm(data.table.races$timeW ~ data.table.races$distance)
predict.women.record.distance <- predict(fit.women.record.distance)

# Call:
#   lm(formula = data.table.races$timeW ~ data.table.races$distance)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -42.821 -11.945  -3.642   8.618  72.211 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 3.1076     4.5367   0.685    0.496    
# data.table.races$distance   5.8684     0.2229  26.330   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.56 on 66 degrees of freedom
# Multiple R-squared:  0.9131,	Adjusted R-squared:  0.9118 
# F-statistic: 693.3 on 1 and 66 DF,  p-value: < 2.2e-16

# to see the correlation
# cor(climb, distance)

# c) scatter plot and report the prediction equation 

# predict timeM by using timeW
fit.men.record.time <- lm(data.table.races$timeM ~ data.table.races$timeW)

# achieve the predictions using the model above
predict.men.record <- predict(fit.men.record.time)

scatterplot.timeMW <- ggplot(mapping = aes(x = data.table.races$timeW,
                                            y = predict.men.record), color = "#69b3a2") + geom_point()
# predict the value by using time" = 490.05 minutes
# 423.9448
timeM.based.timeW490 <- fit.men.record.time$coefficients %*% c(1, 490.05)

# d) find the correlation between timeM and timeW
cor.test(data.table.races$timeW, data.table.races$timeM, method = "kendall")


# e)
fit.men.record.time.another <- lm(timeM ~ -1 + timeW, data = data.table.races)

predict.men.record.noerror <- predict(fit.men.record.time.another)

scatterplot.timeMW.noerror <- ggplot(mapping = aes(x = data.table.races$timeW,
                                           y = predict.men.record.noerror), color = "#69b3a2") + geom_point()

####################
#######TASK16#######
####################

# load the race data
data.table.florida <- read.csv("Florida.dat", sep = "")

# p value shoukd be small less than 0.05

# marginal model fit by HS(high school)
fit.education.urb.marginal <- lm(Urban ~ HS, data = data.table.florida)

# total model fit by everyone
fit.education.urb.conditional <- lm(Urban ~ HS + Crime + Income, data = data.table.florida)

# correlation between crime and education
cor.crime.edu <- cor(data.table.florida$Crime, data.table.florida$HS)

####################
#######TASK17#######
####################

# load UN data
data.table.UN <- read.csv("UN.dat", sep = "")

# multiple regression predicting Internet
fit.Internet.all <- lm(Internet ~ GDP + HDI + GII + Fertility + CO2 + Homicide + Prison, data = data.table.UN)

# multiple regression predicting Internet just by using GDP
fit.Internet.GDP <- lm(Internet ~ GDP , data = data.table.UN)

# R^2 : a corrected goodness-of-fit (model accuracy) measure for linear models


library(VGAM)





