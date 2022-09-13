####################
#######TASK18#######
####################

solar.data.task18 <- read.csv2("Solar.csv", stringsAsFactors=TRUE)

solar.data.task18$batch <- as.factor(solar.data.task18$batch)

write.csv(solar.data.task18, file = "Solar-Task-10-Preprocessed.csv", row.names = FALSE)

#Boxplot
#X-Axis : batch
#Y-Axis : Pmax
solar.pmax.boxplot.batches <- ggplot(mapping = aes(x = solar.data.task18$batch,
                                                   y = solar.data.task18$Pmax)) + aes(color = solar.data.task18$batch)  + geom_boxplot() + theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size = 5))

# ANOVA of the data regarding Pmax based on batch
anova.solar.pmax.batches <- aov(Pmax ~ batch, data = solar.data.task18)

# c) ANOVA
# summary of the ANOVA
#summary(anova.solar.pmax.batches)
# Levene's test:
# verify if every or subset of observations still have same variance 
# as the whole
Pmax <- solar.data.task18$Pmax
batch <- solar.data.task18$batch

levene.test.pmax.batch <- leveneTest(Pmax, batch)