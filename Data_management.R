#Get the csv from Github
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/comic-characters/dc-wikia-data.csv")
dc <- read.csv(text = x)
y <- getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/comic-characters/marvel-wikia-data.csv")
marvel <- read.csv(text = y)

#Load packages
library(statsr)
library(dplyr)
library(ggplot2)
library(compare)
library(RColorBrewer)

#List variables
names(dc)
names(marvel)

#Create identifying columns in each dataframe
dc$universe <- "dc"
marvel$universe <- "marvel"

#Append dataframes
class(dc)
class(marvel)
compare(colnames(marvel), colnames(dc))
characters <- rbind(dc, marvel)
class(characters)
summary(characters)

characters %>%
  summarise(mean(Year, na.rm=TRUE))

#Make some plots
ggplot(data=characters, aes(x=characters$universe)) + stat_count()
ggplot(data=dc, aes(x=dc$Year, fill=ALIGN)) + geom_histogram(binwidth=5)
ggplot(data=marvel, aes(x=marvel$Year, fill=ALIGN)) + geom_histogram(binwidth=5)
ggplot(data=characters, aes(x=SEX, y=Year)) + geom_violin()   
ggplot(data=ForDaveNonmissing, aes(x=characters.ALIGN, y=characters.Year, fill=characters.ALIGN))+ geom_violin()+ggtitle("GSM Comic Book Characters")+xlab("Alignment")+ylab("Year")+theme(legend.position="none", text=element_text(size=15))+scale_fill_manual(values=c("#51A7F9", "#70BF41", "#737476", "#EF5C61"))

#Run some chisq tests for correlation of nominal variables
Hair2Eyes <- table(characters$HAIR, characters$EYE)
chisq.test(Hair2Eyes)

Hair2Alignment <- table(characters$HAIR, characters$ALIGN)
chisq.test(Hair2Alignment)

Sex2Alive <- table(characters$SEX, characters$ALIVE)
chisq.test(Sex2Alive)

Sex2Alive

Sex2Sex <- table(characters$SEX, characters$SEX)
chisq.test(Sex2Sex)

Sex2Universe <- table(characters$SEX, characters$universe)
chisq.test(Sex2Universe)

Sex2Universe

Eyes2Alive <- table(characters$EYE, characters$ALIVE)
chisq.test(Eyes2Alive)

Align2GSM <- table(characters$ALIGN, characters$GSM)
Align2GSM

#Make a table for Dave of characters with non-heterosexual identities
ForDave <- data.frame(characters$name, characters$ALIGN, characters$GSM, characters$Year, characters$universe)
ForDaveNonmissing <- subset(ForDave, ForDave$characters.GSM!="")
write.csv(ForDaveNonmissing, file = "GSM_comic_book_characters.csv")

#Are GSM characters more likely to be dead than non-GSM characters?
#Create dummy variables
characters$alive_dummy <- factor(with (characters, ifelse((ALIVE=="Living Characters"), 1, 0)))
characters$gsm_dummy <- factor(with (characters, ifelse((GSM!=""), 1, 0)))
characters$good_dummy <- factor(with(characters, ifelse((ALIGN=="Good Characters"), 1, 0)))
characters$male_dummy <- factor(with(characters, ifelse((SEX=="Male Characters"), 1, 0)))

#Run tests across whole pop
Alive2GSM <- table(characters$alive_dummy, characters$gsm_dummy)
Alive2GSM
27/(27+127)
5437/(5437+17681)
chisq.test(Alive2GSM) #Not sig
fisher.test(Alive2GSM) #Not sig

#Try a logit regression
alive_prob <-glm(alive_dummy ~ gsm_dummy + good_dummy + male_dummy + Year, data=characters, family=binomial())
summary(alive_prob)
str(summary(alive_prob))
#Hmmmmmm. Let's see if the fit is better without gsm.
alive_prob_reduced <-glm(alive_dummy ~ good_dummy + male_dummy + Year, data=characters, family=binomial())
summary(alive_prob_reduced)
#Comparing the two
anova(alive_prob_reduced, alive_prob, test="Chisq")

#Let's try a random forest model
#First, let's fill in missing variables
summary(characters)
summary(characters$Year)
summary(characters$ID)
#Create new dataset with complete Year variables
RFModel <- subset(characters, characters$Year!="")
