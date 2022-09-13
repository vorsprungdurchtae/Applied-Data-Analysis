library(ggplot2)
library(mvtnorm)
library(MASS)
library(dplyr)
library(tidyr)
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

inv_pseudo_matrix_task6_2 <- solve(pseudo_matrix_task6_2)

###################
#######TASK7#######
###################
# a) import the both csv data 
data.frame.survey1a <- read.table("survey1a.csv", header = TRUE, sep = ";")

data.frame.survey1b <- read.table("survey1b.csv", header = TRUE, sep = ";")

# b) transform the measured dimensions and the mean score to tpye numeric
# sapply() function takes list, vector or data frame as input and gives output in vector or matrix.
data.frame.survey1a[,5:ncol(data.frame.survey1a)] <- sapply(data.frame.survey1a[,5:ncol(data.frame.survey1a)], as.numeric)
data.frame.survey1b[,5:ncol(data.frame.survey1b)] <- sapply(data.frame.survey1b[,5:ncol(data.frame.survey1b)], as.numeric)

# c) merge two dataframes

# merge the two dataframes
data.frame <- rbind(data.frame.survey1a, data.frame.survey1b)

# fill the NA values of each columne
# bad_colnames <- names(which(colSums(is.na(data.frame)) > 0))
# fill(data.frame, .direction = "downup")
# %>% imports dplyr
data.frame <- data.frame %>% fill(names(data.frame), .direction = "downup")

# filter the duplicated rows 
data.frame <- data.frame %>% filter(duplicated(data.frame))

# d) scatterplot X : Age, Y : DimSchool diff: SEX 

# dataframe only man
df_m <- data.frame[data.frame$Sex == 'm', ]

# dataframe only girls
df_f <- data.frame[data.frame$Sex == 'f', ]

#single plotting
plot(df_m$Age, df_m$DimSchool, pch = 20, col = "blue")
plot(df_f$Age, df_f$DimSchool, pch = 20, col = "red")

#base_plot <- base_plot +
#aes(color = Home.Value)
age_dimschool_plot <- ggplot(mapping = aes(x = data.frame$Age,
                     y = data.frame$DimSchool))
age_dimschool_plot <- age_dimschool_plot + aes(color = data.frame$Sex) + geom_point()

age_dimfriends_box_plot <- ggplot(mapping = aes(x = data.frame$Age,
                                                y = data.frame$DimFriends))

age_dimfriends_box_plot <- age_dimfriends_box_plot + aes(color = data.frame$Sex) + geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)

# e) save the data.frame to .RData
write.csv(data.frame, file = "df_task7", row.names = FALSE)


###################
#######TASK8#######
###################
# a) load credits.wsv
data.frame.credits <- read.table("credits.wsv", header = TRUE, sep = " ")

# b) switch the binary variable gastarb 2 from to 1, 1 to 2 
data.frame.credits$gastarb <- 3 - data.frame.credits$gastarb

# c) using cut
# time (0, 6] 10,  (6, 12] 9,  (12, 18] 8, ... ,  (58, inf] 0,   
# into dtime column

#x <- c(5, 2, 6, 4, 1, 2, 2, 5, 4, 4, 6, 4, 2, 5, 5, 3, 6, 1, 4, 5)

#interval = (0:3)
#label = (1:3)

#cuttingx <- cut(x/2, breaks = interval, labels = label)

time_score_cuts = c(0, 6,  12, 18,  24, 30,   36, 42, 48, 54, Inf)
time_score_label = c (10, 9,  8,  7,   6,   5,   4,  3,  2,  1)
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


data.frame.credits$simple.score = colSums(data.frame.credits[,to_sum_columns])

write.csv(data.frame.credits, file = "df_task8", row.names = FALSE)

