#########
#TASK 29
#########

# (a) Download the file FieldGoal.csv from RWTHmoodle 
# and load it as a data frame into your workspace.

FG = read.csv("FieldGoal.csv", header = TRUE, sep = ";")

# (b) Fit a logistic regression model that predicts Good. 
# using Dist as explanatory variable and plot the predicted probabilities 
# for a good kick as a function of Dist.

FG.fit = glm(Good. ~ Dist, data = FG, family = binomial(link = logit))

plot.fit = function(x, coeff){
  
  return( exp(coeff[1] + x*coeff[2])/(1 + exp(coeff[1] + x*coeff[2])) )
  
}

x = min(FG$Dist):max(FG$Dist)

plot(x, 
     plot.fit(x, FG.fit$coefficients), 
     type="l", 
     xlab = "Distance", 
     ylab = "plot.fit", 
     col = "red")

# (c) Calculate the percentage of correct classified observations 
# according to the model in (b).

Good.pred = predict(FG.fit, type = "response") > 0.5

sum(FG$Good. == Good.pred)/nrow(FG)

# (d) Test at significance level α = 0.05 the hypotheses
# H0 :β1 =0 versus H1 :β1 ∕=0



