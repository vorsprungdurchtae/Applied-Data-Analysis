######
# task 1
######

gm <- rep(c("for", "against"), 3)
income <- c("high", "high", "middle", "middle", "low", "low")
count <- c(320, 150, 280, 166, 258, 112)
contigency =xtabs(count~gm+income)
contigency
data <- data.frame(gm, income, count)
data

# (a)

470/1286 # 0.3654743
428/1286 # 0.3328149

# (b)
# (i)
glm.ind = glm(count~gm+income, family=poisson, data=data)
summary(glm.ind)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   5.05256    0.06072  83.216  < 2e-16 ***
#  gmfor         0.69548    0.05918  11.753  < 2e-16 ***
#  incomelow    -0.23923    0.06950  -3.442 0.000577 ***
#  incomemiddle -0.05241    0.06610  -0.793 0.427840    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#(Dispersion parameter for poisson family taken to be 1)
#Null deviance: 164.5556  on 5  degrees of freedom
#Residual deviance:   4.9923  on 2  degrees of freedom
#AIC: 55.822

res.p = residuals(glm.ind, type="pearson") 
xtabs(res.p ~ gm+income, data=data)
#         income
#gm              high        low     middle
#against -0.5135571 -1.0040199  1.4416770***
#for      0.3627162  0.7091214 -1.0182308
# (ii)
chisq.test(contigency)$statistic
#X-squared 
#5.02144 

# (c)
# (i)
glm.ind$deviance # 4.992269

# (ii)
p.value <- 1-pchisq(glm.ind$deviance, glm.ind$df.residual)
p.value
# 0.08240293

# (d)
glm.sat = glm(count ~ gm * income, family=poisson, data=data)
glm.sat
summary(glm.sat)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         5.01064    0.08165  61.368  < 2e-16 ***
#  gmfor               0.75769    0.09895   7.657  1.9e-14 ***
#  incomelow          -0.29214    0.12488  -2.339   0.0193 *  
#  incomemiddle        0.10135    0.11265   0.900   0.3683    
#gmfor:incomelow     0.07678    0.15032   0.511   0.6095    
#gmfor:incomemiddle -0.23488    0.13924  -1.687   0.0916 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#(Dispersion parameter for poisson family taken to be 1)
#Null deviance: 1.6456e+02  on 5  degrees of freedom
#Residual deviance: 1.1102e-14  on 0  degrees of freedom
#AIC: 54.83

# (e)
# model selection
glm.select  = step(glm.sat, direction="backward")
summary(glm.select)

#Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)         5.01064    0.08165  61.368  < 2e-16 ***
#  gmfor               0.75769    0.09895   7.657  1.9e-14 ***
#  incomelow          -0.29214    0.12488  -2.339   0.0193 *  
#  incomemiddle        0.10135    0.11265   0.900   0.3683    
#  gmfor:incomelow     0.07678    0.15032   0.511   0.6095    
#  gmfor:incomemiddle -0.23488    0.13924  -1.687   0.0916 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for poisson family taken to be 1)

#Null deviance: 1.6456e+02  on 5  degrees of freedom
#Residual deviance: 1.1102e-14  on 0  degrees of freedom
#AIC: 54.83

# (f)
glm.select$coefficients[1]+ 1.96*0.08165 # 5.170669 
glm.select$coefficients[1]- 1.96*0.08165 # 4.850601 



######
# task 2
######
house <- read.delim("C:/edu/rwth-aa/rwth-repo/ADA/r-lab/src/Houses.txt")
house$new <- as.factor(house$new)
house

house_a <- house[house$taxes > 3000 & house$bedrooms >=2,] # 17

# (b)

glm_b <- glm(price ~ size+new+taxes+bedrooms+baths, data = house, family = binomial)
summary(glm_b)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)   
#  (Intercept) -4.9529176  1.6562000  -2.991  0.00278 **
#  size         0.0032981  0.0011257   2.930  0.00339 **
#  new1         1.0313654  1.3121936   0.786  0.43188   
#  taxes        0.0008784  0.0004333   2.027  0.04267 * 
#  bedrooms    -0.7826233  0.5842793  -1.339  0.18042   
#  baths        0.3416320  0.6916827   0.494  0.62137   

# (c)


#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#  taxes        0.0008784  0.0004333   2.027  0.04267 * 

taxes_coef = glm_b$coefficients[4]
se = summary(glm_b)$coefficients[4, 2]
se

z.squ <- (taxes_coef / se)^2 # Std. Error / z value^2
pval = 1-pchisq(z.squ , df=1)
pval # 0.04266548 > 0.01 not reject Ho

# (d)
test <- data.frame(new=1, size=1000, taxes=1200, bedrooms=2, baths=2)
test$new <-as.factor(test$new)
pred_glm_b <- predict.glm(glm_b, newdata = test, type = "response")
pred_glm_b # 0.3890287 

# (e) confident interval
glm_b_conf <- confint(glm_b, level=0.95) 

#               2.5 %       97.5 %
#(Intercept) -8.535482e+00 -1.957809257
#size         1.255701e-03  0.005722842
#new1        -1.366495e+00  4.215957243
#taxes        7.951024e-05  0.001811950
#bedrooms    -1.995860e+00  0.328357772
#baths       -9.688257e-01  1.809653239



# (d)
library(pROC)

roc.curve1=roc(price~fitted(glm_b), data=house)
auc(roc.curve1) # Area under the curve: 0.88




