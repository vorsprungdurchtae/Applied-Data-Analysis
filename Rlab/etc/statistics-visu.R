library(ggplot2)
library(mvtnorm)
library(MASS)
library(dplyr)
library(tidyr)
library(gridExtra)
library(car)
library(datasets)

# 
# # Create a, b, c, d variables
# a <- c(10,20,30,40)
# b <- c('book', 'pen', 'textbook', 'pencil_case')
# c <- c(TRUE,FALSE,TRUE,FALSE)
# d <- c(2.5, 8, 10, 7)
# # Join the variables to create a data frame
# df <- data.frame(a,b,c,d)
# df

# generate a dataframe such that
# each column represents the attributes
# and each row represents corresponding event logs
# the row order is L_filtered, L_A, L_W, and L_O

num_case = c(28976, 28976, 28976)
num_variant = c(88, 197, 815)
num_events = c(220481, 137037, 178612)
num_avg_event = c(7.61, 4.73, 6.16)
num_activity = c(10, 6, 8)

log_names = c("L_A", "L_W", "L_O")

df <- data.frame(log_names,
                 num_case,
                 num_variant,
                 num_events,
                 num_avg_event,
                 num_activity)

colnames(df) <- c('Log' ,'Cases','Variants','Events', 'Avg.Events', 'Activities')



#df$Log <- factor(df$Log, levels = df$Log)

ggplot(df,aes(Log,Activities))+
  geom_bar(stat="identity", fill="steelblue")  + 
  labs(x="Log", y="Activities") +
  geom_text(aes(label=Activities), vjust=-0.3,  size=3.5)+
  theme_minimal()


num_case = c(28976, 28976, 28976, 28976)
num_variant = c(6489, 88, 815, 1732)
num_events = c(574638, 220481, 178612, 175545)
num_avg_event = c(19.83, 7.61, 6.16, 6.06)
num_activity = c(24, 10, 8, 6)

log_names = c("Lfiltered", "L_A", "L_O", "L_W")

df <- data.frame(log_names,
                 num_case,
                 num_variant,
                 num_events,
                 num_avg_event,
                 num_activity)

colnames(df) <- c('Log' ,'Cases','Variants','Events', 'Avg.Events', 'Activities')



#df$Log <- factor(df$Log, levels = df$Log)

ggplot(df,aes(Log,Activities))+
  geom_bar(stat="identity", fill="steelblue")  + 
  labs(x="Log", y="Activities") +
  geom_text(aes(label=Activities), vjust=-0.3,  size=3.5)+
  theme_minimal()

