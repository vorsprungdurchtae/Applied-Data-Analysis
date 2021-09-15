library(tidyverse)
library(dplyr)
library(ggplot2)



#############
#
# Task 7
#
#############

# (a)
# read in both data files
data.survey.a = read.csv2("/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab2/Survey1a.csv",stringsAsFactors=TRUE)
data.survey.b = read.csv2("/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab2/Survey1b.csv",stringsAsFactors=TRUE)
# Note: In some R versions, stringsAsFactors is by default TRUE, in others
# it is FALSE. Here we set it explicitly to ensure compatibility

# (b)
# selection of the columns
idx = (regexpr("Dim",names(data.survey.a))>-1)|names(data.survey.a)=="MeanScore"
# transformation from factor to character and from character to numeric
data.survey.a[, idx] <- sapply(sapply(data.survey.a[, idx], as.character), as.numeric)
data.survey.b[, idx] <- sapply(sapply(data.survey.b[, idx], as.character), as.numeric)

# (c)
# select indices of missing values in Survey1b.csv
missing.values = which(is.na(data.survey.b), arr.ind = TRUE)

for(row in missing.values[,1]){
  # assign data.survey.b with the corresponding values from data.survey.a
  # interviews can be identified via Person and Date uniquely
  data.survey.b[row,]=data.survey.a[data.survey.a$Person==data.survey.b$Person[row] & data.survey.a$Date == as.character(data.survey.b$Date[row]), ]
}
data.survey = rbind(data.survey.a, data.survey.b)
# delete duplicated rows
data.survey = data.survey[!duplicated(data.survey), ]


# alternative using the package tidyverse
data.survey = rbind(data.survey.a, data.survey.b)
data.survey = data.survey %>%
  # sort the the data by interviews
  arrange(Person, Date) %>%
  # duplicated interviews are in successive rows
  # fill missing values in data.survey.b with value above from data.survey.a
  fill(DimBody, DimEmotion, DimSelf, DimFamily, DimFriends, DimSchool, MeanScore)
# delete duplicated rows
data.survey = filter(data.survey,duplicated(data.survey)==FALSE)

# (d)
plot(data.survey$Age, data.survey$DimSchool, col=c("red","blue")[data.survey$Sex]);
legend(x="topright", legend = levels(data.survey$Sex), col=c("red","blue"), pch=1)

# alternative using the package tidyverse
ggplot(data.survey)+geom_point(aes(Age, DimSchool, col=Sex))

#(e)
boxplot(data.survey$DimFriends[data.survey$Sex=="m"],data.survey$DimFriends[data.survey$Sex=="f"], xaxt="n",main="Boxplot for dimension: Friends")
axis(1, at=1:2, labels=c('female','male'))

# alternative using the package tidyverse
ggplot(data.survey)+geom_boxplot(aes(DimFriends, col=Sex))

# (f)
save(data.survey,file="/Users/thetruetae/Desktop/RWTH/SS21/ADA/RLab/R-Lab4/Survey1.RData")

print("blu")
