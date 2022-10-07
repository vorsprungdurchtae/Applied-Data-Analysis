#########
#TASK 32
#########

library(vcd)
library(vcdExtra)

# (a) Load the dataset GSS as a contingency table into your workspace.
tab.party=xtabs(count~sex+party, data=GSS)

party.margins = addmargins(tab.party)
party.margins 

# (b) Test whether party affiliation is independent of gender 
# by testing the goodness of fit of the associated log-linear model. 
# What is the corresponding p-value?

glm.ind = glm(count ~ sex + party, family=poisson, data=GSS)
glm.ind
summary(glm.ind)

# asymptotic p-value
p.val = 1 - pchisq(glm.ind$deviance,df=glm.ind$df.residual)
p.val

# Chi-squared Test
chisq.test(tab.party)

# saturated model
glm.sat = glm(count ~ sex * party, family=poisson, data=GSS)
glm.sat
summary(glm.sat)

# (c)
# Pearsonian residuals and corresponding standardized residuals
res.p = residuals(glm.ind, type="pearson") 
stdres.p = rstandard(glm.ind, type="pearson") 
xtabs(res.p ~ sex + party, data=GSS)
stdres.p.tab = xtabs(stdres.p ~ sex + party, data=GSS)
stdres.p.tab

# Comparison with the statistic of the Chi-squared Test in (b)
sum(res.p^2)
chisq.test(tab.party)$statistic

# deviance residuals and corresponding standardized residuals
res.d = residuals(glm.ind, type="deviance")
stdres.d = rstandard(glm.ind, type="deviance")
xtabs(res.d ~ sex + party, data=GSS)
stdres.d.tab = xtabs(stdres.d ~ sex + party, data=GSS)
stdres.d.tab

# Comparison with the deviance of the log linear model
summary(glm.ind)
sum(res.d^2)

# own implementation
2 * (sum(GSS$count * log(GSS$count/glm.ind$fitted.values))) 

# (d)
# mosaic-plot
# standardized Pearsonian residuals
mosaic(tab.party,gp=shading_Friendly,residuals=stdres.p.tab,
       residuals_type="Std\nresiduals",labeling=labeling_residuals)
# standardized deviance residuals
mosaic(tab.party,gp=shading_Friendly,residuals=stdres.d.tab,
       residuals_type="Std\nresiduals",labeling=labeling_residuals)

#############
#High School
#############

NI = 3
NJ = 5
freq = c(45,116,19,48,23,40,167,33,68,41,47,185,34,63,26)
row.lb <- c("too little","about right","too much")
col.lb <- c("LT HS","HS", "JColg","BA", "Grad")
WELFARE <- gl(NI,NJ,length=NI*NJ, labels=row.lb)
DEGREE <- gl(NJ,1,length=NI*NJ, labels=col.lb)
nt.frame <- data.frame(freq,WELFARE,DEGREE)

I.glm = glm(freq~WELFARE+DEGREE, family=poisson, data = nt.frame)

MLEs = xtabs(I.glm$fitted.values ~ WELFARE + DEGREE, data = nt.frame)
stdres = xtabs(rstandard(I.glm) ~ WELFARE + DEGREE, data = nt.frame)

#######################################
#Smoking and Depressive Disorder
#######################################

freq <- c(40, 10, 889, 417, 104, 40, 840, 873)
row <- rep(1:2, 4)
col <- rep(1:2, each=2,2)
lay <- rep(1:2, each=4)
row.lb <- c("yes","no")
col.lb <- c("yes","no")
lay.lb <- c("male", "female")
S <- factor(row,labels=row.lb)
D <- factor(col,labels=col.lb)
G <- factor(lay, labels=lay.lb)
depres.fr <- data.frame(freq,S,D,G)

saturated = glm(freq ~ S*D*G, poisson, data = depres.fr)
step(saturated, direction = "backward")

hom.assoc = glm(freq ~ S*D + D*G + S*G, poisson, data = depres.fr)

#########
#TASK 33
#########


# (a)
accident = read.csv("accident.csv", header = TRUE, sep = ";")

# transform to a contingency table
accident.tab = table(accident)
accident.tab
# add margins
addmargins(accident.tab)

# transform to a dataframe
accident.dat = data.frame(accident.tab)
accident.dat

# (b)
accident.fit = glm(Freq ~ speedlimit*year*street, poisson, data = accident.dat)
accident.AIC = step(accident.fit, direction = "backward")

accident.res.p= residuals(accident.AIC, type="pearson")
accident.stdres.p = rstandard(accident.AIC, type = "pearson")


accident.res.d = residuals(accident.AIC, type="deviance")
accident.stdres.d = rstandard(accident.AIC, type = "deviance")


xtabs(accident.res.p ~ speedlimit + year + street, data=accident.dat)
xtabs(accident.stdres.p ~ speedlimit + year + street, data=accident.dat)

xtabs(accident.res.d ~ speedlimit + year + street, data=accident.dat)
stdres.d.tab = xtabs(accident.stdres.d ~ speedlimit + year + street, data=accident.dat)

# standardized deviance residuals
mosaic(accident.tab,gp=shading_Friendly,residuals=stdres.d.tab,
       residuals_type="Std\nresiduals",labeling=labeling_residuals)

mosaic(accident.tab,gp=shading_Friendly,residuals=stdres.p.tab,
       residuals_type="Std\nresiduals",labeling=labeling_residuals)



#########
#TASK 34
#########

# (a) transfer the data into R Workspace
freq = c(50, 24, 9, 6, 41, 14, 4, 1, 315, 4012, 40, 459, 147, 1594, 11, 124)
G.lb = c("≤ 260", "> 260")
S.lb = c("< 5", "> 5")
A.lb = c("< 30", "30 +")
G.ind <- rep(1:2, 8)
S.ind <- rep(1:2, each=2,4)
A.ind <- rep(1:2, each=4,2)
G = factor(G.ind, labels = G.lb)
S = factor(S.ind, labels = S.lb)
A = factor(A.ind, labels = A.lb)
I.lb = c("No", "Yes")
I.ind = rep(1:2, each=8)
I = factor(I.ind, labels = I.lb)
# S <- factor(row,labels=row.lb)
# D <- factor(col,labels=col.lb)
# G <- factor(lay, labels=lay.lb)
# depres.fr <- data.frame(freq,S,D,G)

gest.fr = data.frame(freq, A, S, G, I)

# (c) Fit the models 
# (AGIS),
fit.AGIS = glm(freq ~ A*G*I*S, poisson, data = gest.fr)
fit.AGIS.AIC = step(fit.AGIS, direction = "backward")

fit.second = glm(freq ~ A*G*I + A*I*S + A*G*S + G*I*S, poisson, data = gest.fr)
fit.second.AIC = step(fit.second , direction = "backward")

fit.third = glm(freq ~ A*G + A*I + A*S + G*I + G*S + I*S, poisson, data = gest.fr)
fit.third.AIC = step(fit.third, direction = "backward")

fit.fourth = glm(freq ~ A*S + G + I, poisson, data = gest.fr)
fit.fourth.AIC = step(fit.fourth, direction = "backward")

#(AGI, AIS, AGS, GIS),

#(AG,AI,AS,GI,GS,IS) and 

#(AS,G,I).

