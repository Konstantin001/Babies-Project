## Babies Data Aggregation and Cleaning
## Data: './data/Raw'
library(stringr)

## Set Working Directory
setwd('/Users/konstantintskhay/Desktop/Projects/Finished Projects/2015/Babies/Study 6 - Final')

## Identify files in the directory
files <- list.files('./data/Raw', full.names = TRUE)

## Merge Categorization data into a single dataset
catData <- list()

for (i in 1:length(1:57)) {
        catData <- rbind(catData, as.character((read.csv(files[i])[ , 8])))
}

catDF <- data.frame(matrix(unlist(catData), nrow=57))

## Add names to the columns in the data
names(catDF) <- as.character(read.csv('data/ColNames.csv', header = FALSE)[, 4])

## add file.id as an additional identifier 
catDF$file.id <- str_replace_all(list.files('./data/Raw/'), ".csv", "")
row.names(catDF) <- seq(1:length(files))

## identify people who did not finish by examining the catDF--visaul examination
## remove: 8, 11, 14, 18, 25
remove <- c(8, 11, 14, 18, 25)
catDF <- catDF[-remove,]

## take a look at the data
str(catDF)

## translate ages after visual inspection to numbers
catDF[c(1, 30), 101] <- c(19, 18); catDF[, 101]

## translate the exposure times to numbers
catDF[c(5, 13, 24, 30, 31, 35, 42, 46), 102] <- c(1, 0, 0, 2, 1, 0, 1, 0); catDF[, 102]

## reinitialize the row numbering
row.names(catDF) <- seq(1:(length(files)-length(remove)))

##### Categorization data are ready: catDF.csv
write.table(catDF, file = './data/catDF.csv', sep = ',', row.names = FALSE)


##### Reaction Time (RT) data

## Aggregate all reaction times
RTData <- list()

for (i in 1:length(1:57)) {
        RTData <- rbind(RTData, as.character((read.csv(files[i])[ , 10])))
}

RTDF <- data.frame(matrix(unlist(RTData), nrow=57))

## add names to the Dataset
names(RTDF) <- as.character(read.csv('data/ColNames.csv', header = FALSE)[, 4])

## add additional identifier -- file.id
RTDF$file.id <- str_replace_all(list.files('./data/Raw/'), ".csv", "")

## remove people who did not finish--same people in catDF:
## remove:
remove <- c(8, 11, 14, 18, 25)
RTDF <- RTDF[-remove,]
row.names(RTDF) <- seq(1:(length(files)-length(remove)))

## Move demographics variable responses from catDF to RTDF 
## (NOTE: RTDF contained only reaction times until now)
RTDF[, 98:104] <- catDF[,98:104]

## Export the RTDF dataset with reaction times as a .csv file
write.table(RTDF, file = './data/RTDF.csv', sep = ',', row.names = FALSE)

## Data cleaning: need to clean RTDF.csv
## Use catDF to identify people who categorized the faces incorrectly
## replaces all incorrect with NA in catDF
dummy <- catDF

for (i in c(2:25, 50:73)) {
        dummy[, i] <- with(dummy, ifelse(dummy[, i] == 'Z', 'NA', '/'))
}
for (i in c(26:49, 74:97)) {
        dummy[, i] <- with(dummy, ifelse(dummy[, i] == '/', 'NA', 'Z'))
}

sum((dummy[,]=='NA')) ## compute how many people categorized faces incorrectly

## replace all incorrect RT with NA in RTDF
dummy2 <- RTDF

for (i in 2:97) {
        for (j in 1:52) {
                if (dummy[j, i] == "NA") {
                        dummy2[j, i] <- NA
                }
        }
}

RTDF2 <- dummy2

## computes the grand avergae reaction time
mean(as.numeric(as.character(RTDF2[, 2:97])), na.rm = TRUE)  
                                                                
## RTDF now contains only correct answers

## convert all RTs to numeric
for (i in 2:97) {
        RTDF2[, i] <- as.numeric(levels(RTDF2[, i])[RTDF2[, i]])
}

## removing outliers from the RTDF2 data.frame

## calculate mean + 3sd for each row: 
RTDF2$sd3 <- rowMeans(RTDF2[, 2:97], na.rm = TRUE) + 3*transform(RTDF2[, 2:97], SD = apply(RTDF2[, 2:97], 1, sd, na.rm = TRUE))$SD

## remove low scores -- people did not spend much time on the task (300 ms)
for (i in 2:97) {
        for (j in 1:52) {
                if (is.na(RTDF2[j, i])) {
                } 
                else if (RTDF2[j, i] <= 300) {
                        RTDF2[j, i] <- NA
                }
        }
}


## identified the number of trials that were less than 300ms
## dummy2 to numeric
for (i in 2:97) {
        dummy2[, i] <- as.numeric(levels(dummy2[, i])[dummy2[, i]])
}

## How many less or equal to 300ms 
sum(dummy2[2:97] <= 300, na.rm = TRUE) 
## there were 31 values that are less or equal to 300ms

## Count how many are greater than 3 sd away from the mean?
counter = 0
for (i in 2:97) {
        for (j in 1:52){
                if(is.na(dummy2[j, i])){
                         } else if(dummy2[j, i] >= RTDF2[j, 105]) {
                         counter = counter +1
                }
        }
}

## 96 greater than 3sd over the individual mean

## Remove greater than 3sd from each Ps mean
for (i in 2:97) {
        for (j in 1:52) {
                if (is.na(RTDF2[j, i])) {
                } 
                else if (RTDF2[j, i] >= RTDF2[j, 105]) {
                        RTDF2[j, i] <- NA
                }
        }
}


## Get Means and Medians for each condition: gg, gb, bg, bb ##
RTDF3 <- transform(RTDF2, 
                   ggMed = apply(RTDF2[, 2:25], 1, median, na.rm = TRUE), 
                   gbMed = apply(RTDF2[, 26:49], 1, median, na.rm = TRUE),
                   bgMed = apply(RTDF2[, 50:73], 1, median, na.rm = TRUE),
                   bbMed = apply(RTDF2[, 74:97], 1, median, na.rm = TRUE),
          
                   ggMean = apply(RTDF2[, 2:25], 1, mean, na.rm = TRUE), 
                   gbMean = apply(RTDF2[, 26:49], 1, mean, na.rm = TRUE),
                   bgMean = apply(RTDF2[, 50:73], 1, mean, na.rm = TRUE),
                   bbMean = apply(RTDF2[, 74:97], 1, mean, na.rm = TRUE)
          )

## recreate a data.frame with necessary variable ordering
sumDF <- RTDF3[, c(99:102, 104, 106:113)]
names(sumDF)

## reshape into long data format ##
sumMed <- reshape(sumDF[1:9], 
             varying = names(sumDF[6:9]), 
             v.names = "median",
             timevar = "condition", 
             times = names(sumDF[6:9]), 
             direction = "long")

sumMean <- reshape(sumDF[10:13], 
                  varying = names(sumDF[10:13]), 
                  v.names = "mean",
                  timevar = "condition", 
                  times = names(sumDF[10:13]), 
                  direction = "long")

finalDF <- sumMed
finalDF$mean <- sumMean$mean

## check the final data
names(finalDF)
str(finalDF)

## add row names
row.names(finalDF) <- seq(1:208)

## Reformatting the variables
finalDF$sex <- as.numeric(as.character(finalDF$sex))
finalDF$race <- as.character(finalDF$race)
finalDF$age <- as.numeric(as.character(finalDF$age))
finalDF$exposure <- as.numeric(as.character(finalDF$exposure))

finalDF$face <- with(finalDF, ifelse(substr(finalDF$condition, 1 ,1) == "g", -.5, .5))
finalDF$name <- with(finalDF, ifelse(substr(finalDF$condition, 2 ,2) == "g", -.5, .5))
finalDF$sex <- with(finalDF, ifelse(sex == 1, .5, -.5))

## remove redundant column
finalDF$condition <- NULL

## extract only the necessary variables
finalDF <- with(finalDF, 
                data.frame(id, file.id, sex, face, name, mean, median, race, age, exposure))     

## check
head(finalDF)

## make file.id and race character vectors
finalDF$file.id <- as.character(finalDF$file.id)
finalDF$race <- as.character(finalDF$race)

## output the final dataset--ready for analysis: final.csv
write.table(finalDF, file = './data/final.csv', sep = ',', row.names = FALSE)
        
## Script End   