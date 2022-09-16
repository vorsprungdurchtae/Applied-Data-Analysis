######
#Task5
######

library(mvtnorm)

#(a) Draw random samples of size n = 30, 100, 300 from a N (μ, σ2) distribution 
#with μ = 5 and σ2 = 4 and create a histogram for each sample size n.

n30 = rnorm(30, 5, 2)
n100 = rnorm(100, 5, 2)
n300 = rnorm(300, 5, 2)

#> hist(n30)
#> hist(n100)
#> hist(n300)

#(b) Add a density estimation using the function density and 
#the probability density function of a N(5,4) distribution to the histograms 
# using different colors. What do you observe?

######
#Task6
######

#(a) Draw random samples of size n = 100 from a N4(0, I4) distribution.
#Hint: You may use the functions rnorm and matrix or the function rmvnorm 
#from the package mvtnorm.

mat_sample = rmvnorm(100, mean = rep(0, 4), sigma = diag(4))

#(b) Initialize a vector μ = (1, 0, 2, −1)′ and matrices

mu = c(1, 0, 2, -1)

sig1 = matrix(c(4, 2, 2, 3,
                2, 3, 2, 1,
                2, 2, 5, 2,
                3, 1, 2, 3), nrow = 4, ncol = 4)

sig2 = matrix(c(4.5, 4.75, 2, 2.25,
                4.75, 5.25, 2.75, 3.25,
                2, 2.75, 2.75, 3.5,
                2.25, 3.25, 3.5, 4.5), nrow = 4, ncol = 4)

#(c) Transform the random vectors from (a) to a sample from a N4(μ, Σ1) 
# distribution and a sample from a N4(μ, Σ2) distribution. 
# Do not generate new random numbers! 
# singular-value decomposition instead.
# Remark: R computes the singular-value decomposition numerically. 
# Replace eigenval- ues smaller than sqrt(.Machine$double.eps)= 1.490116*10−08
# with 0.

# TODO
# 0. Transform the mat_sample such that 
# it is Gaussian distributed with mean = mu, Var = sig1, sig2
# 1. extract the SVD of Sig1 and Sig2 like sig = V * A * VT
# 2. get the sqrt(A)
# 3. multiply V*sqrt(A) to the mat_sample

sig1_sqrt = svd(sig1)$u %*% matrix(diag(sqrt(svd(sig1)$d)), nrow = 4, ncol = 4) %*% t(svd(sig1)$v)

transform1 = sig1_sqrt %*% t(mat_sample) + mu

#replace(x1,x1==5,10)

eig_val_sig2 = replace(svd(sig2)$d, svd(sig2)$d < sqrt(.Machine$double.eps), 0)

sig2_sqrt = svd(sig2)$u %*% matrix(diag(sqrt(eig_val_sig2)), nrow = 4, ncol = 4) %*% t(svd(sig2)$v)

transform2 = sig2_sqrt %*% t(mat_sample) + mu

# (d) Create three scatterplot matrices - one for each sample. 
# What do you observe?

pairs(t(transform1))
pairs(t(transform2))

# (e) Compute the Moore-Penrose general inverse of Σ1 and Σ2. 
# If the inverse of Σ1 and Σ2 exists, 
# does it coincide with the Moore-Penrose general inverse?
# yes! if the matrix has the full rank, (all column vectors are independent)
# then there is an unique inverse matrix
# otherwise, there are a number of inverse matrix(ginv)
library(MASS)
# The case of sig1, the both are same
# ginv(sig1) == solve(sig1)

# The case of sig2, 

######
#Task7
######

#(a) Download the CSV-files Survey1a.csv and Survey1b.csv 
# from the RWTHmoodle space of the course Applied Data Analysis and 
# import the data as a data.frame object into the R workspace.

survey1a = read.csv("Survey1a.csv", header = TRUE, sep = ";")
survey1b = read.csv("Survey1b.csv", header = TRUE, sep = ";")

# (b) Transform the measured dimensions and the mean score of Survey1a.csv 
# and Survey1b.csv to type numeric appropriately.

# TODO: Transform the type of columns with name dim & Mean
# HOWTO?
# extract the index of the column names 
# which(regexpr("Dim|Mean", colnames(survey1a)) == TRUE)
# by using regular expression

idxa = which(regexpr("Dim|Mean", colnames(survey1a)) == TRUE)
survey1a[idxa] = sapply(survey1a[idx], as.numeric)

idxb = which(regexpr("Dim|Mean", colnames(survey1b)) == TRUE)
survey1b[idxb] = sapply(survey1b[idx], as.numeric)

# (c) Create a new data.frame called data.survey 
# that contains the observations of Sur- vey1a.csv and Survey1b.csv. 
# Remember to fill missing values and to remove duplicated observations.
# Hint: You may use the functions arrange, 
# filter and fill from the package dplyr.

# TODO: Fill missing values and remove duplicated obs

survey1a[is.na(survey1a)] = 0
survey1b[is.na(survey1b)] = 0

survey1a = survey1a[!duplicated(survey1a), ]
survey1b = survey1b[!duplicated(survey1b), ]

data.survey = rbind(survey1a, survey1b)

#(d) For the data of data.survey, create an (Age, DimSchool) scatterplot 
# (with the values 
# of Age on the horizontal axis). 
# Differentiate the points by sex with colors. 
# Hint: You may use the package ggplot2

library(ggplot2)

ggplot(data.survey, aes(x=Age, y=DimSchool, color=Sex)) +
  geom_point()

# (e) Create two Box-plots for DimFriends in one figure, 
# one for male and one for female participants.

boxplot(data.survey$DimFriends ~ data.survey$Sex, xlab = "Sex", ylab = "DimFriends")

# (f) Save the data.frame into an .RData file.
write.csv(data.survey,"survey_task7.RData", row.names = FALSE)

######
#Task8
######
# (a) Download the file credits.wsv from RWTHmoodle 
# and import the data as a data.frame object into the R workspace.
credits = read.csv("credits.wsv", header = TRUE, sep = " ")

# (b) Switch the coding for the binary variable gastarb 
# in the data.frame object form 2 to 1 for Gastarbeiter and 
# from 1 to 2 for a native worker.

credits$gastarb = (credits$gastarb - 2) * -1

#brks = c(0, 2, 4, 6)
#lbs = c(1,2,3)

#cut(x, brks, lbs)

time_brk = (0:9)*6
time_brk[11] = Inf
time_lbs = 10:1

credits$dtime = cut(credits$time, time_brk, time_lbs)

amount_brk = c(0,
               500,
               1000,
               1500,
               2500,
               5000,
               7500,
               10000,
               15000,
               20000,
               Inf)

amount_lbs = 10:1

credits$damount = cut(credits$amount, amount_brk, amount_lbs)

age_brk = c(0, 25, 39, 59, 64, Inf)
age_lbs = 1:5
credits$dage = cut(credits$age, age_brk, age_lbs)

repayment_cols = c("account",
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

lm.repayment = lm(repayment ~ 
                    account+
                    dtime+
                    behavior+ 
                    usage+ 
                    damount+ 
                    savings+ 
                    employment+ 
                    rate+ 
                    famgen+ 
                    guaran+ 
                    residence+ 
                    finance+ 
                    dage+ 
                    furthcred+ 
                    home+ 
                    prevcred+ 
                    job+ 
                    pers+ 
                    phone+ 
                    gastarb, data=credits)

# Considering the scores as quantitative variables, 
# create a further variable simple.score by this approach and 
# include it to the data.frame object.

credits$simple.score = fitted(lm.repayment)

# (e) Save the data.frame into a CSV-file.

write.csv(credits,"credits_task8.csv", row.names = FALSE)

