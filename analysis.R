## analysis of the babies data
## Tskhay K. O. 
## Date: Sat Aug 22 11:04:45 2015

## set wd
setwd('/Users/konstantintskhay/Desktop/Projects/Finished Projects/2015/Babies/Study 6 - Final')

## load data
babies <- read.csv('./data/final.csv')

## basic examination of data
names(babies)
str(babies)

## load necessary packages
library(ez)
library(psych)
library(ggplot2)

## Reaction times tend to be skewed - check reaction times skew

skew(babies$mean); hist(babies$mean) ## skew
skew(babies$median); hist(babies$median) ## skew

## Transform reaction times to normal (log transformation)
babies$logMean <- with(babies, log(mean))
skew(babies$logMean); hist(babies$logMean) ## approximately normal

babies$logMedian <- with(babies, log(median))
skew(babies$logMedian); hist(babies$logMedian) ## approximately normal

## Convert name, face, and sex to factor
babies$sex <- factor(babies$sex, levels = c(-.5, .5), labels = c('Female', 'Male'))
babies$name <- factor(babies$name, levels = c(-.5, .5), labels = c('Female', 'Male'))
babies$face <- factor(babies$face, levels = c(-.5, .5), labels = c('Female', 'Male'))

## Add congruence variable to dataset: if name and face same gender = congruent, 
## else = incongruent
babies$cong <- with(babies, ifelse(name == face, "congruent", "incongruent"))

## Hypothesis test: people will be faster when face and name sex match
ezANOVA(data = babies, dv = .(logMedian), wid = id, within = .(face, name))

## include participant sex as covariate -- perhaps women or men are more succeptible
ezANOVA(data = babies, dv = .(logMedian), wid = id, within = .(face, name), between = .(sex))

## Eliminate people with exposure to children
## create new dataset to ensure the integrity of the original data
babies2 <- babies
babies2[babies2$exposure >= 30, ] <- NA
babies2 <- na.omit(babies2)

## Re-run the analyses with new dataset
ezANOVA(data = babies2, dv = .(logMedian), wid = id, within = .(face, name), between = .(sex))

## compute how much time on average people spent categorizing targets (Mean)
mean(babies$mean)

## computes Macrae & Martin (2007) analysis: using congruent in place of 
## name x face interaction
ezANOVA(data = babies, dv = .(logMedian), wid = id, within = .(cong))

## Figure ##

## Get mean's and se's for each condition
cdata <- ddply(babies, c("name", "face"), summarise,
               N    = length(logMedian),
               mean = mean(logMedian),
               sd   = sd(logMedian),
               se   = sd / sqrt(N))

## check the data structure for summaries
cdata

## construct graph using ggplot2
plot1 <- ggplot(cdata, aes(x=name, y=mean, fill=face))

plot1 + 
        geom_bar(position=position_dodge(), stat="identity") + 
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                      width=.2,                    
                      position=position_dodge(.9)) +
        coord_cartesian(ylim=c(6.5,6.75)) +
        xlab("Name") +
        ylab("log Reaction Time") +
        scale_fill_grey(start = .6, end = .3, name = "Face")  +
        theme_bw()

## Output the plot        
dev.copy(pdf, 'Fig1.pdf'); dev.off()

## Script end


