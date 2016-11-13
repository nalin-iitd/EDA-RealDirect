#intall packages
install.packages("ggplot2")
install.packages("doBy")

#load libraries
library(ggplot2)
library("doBy")

#Read 31 files containing nyt csv data and combine whole data into a single variable final data
fileDir <- 'D:/dic_data/problem2/'
fileDir1 <- 'D:/dic_data/problem2/nyt1.csv'
files <- list.files(path = fileDir, pattern = "\\.csv$")
finalData = read.table(fileDir1,header = T, sep = ',')

for(i in 2:31){
  tempDir = paste(fileDir,files[i],sep = "")
  loopFileData = read.table(tempDir,header = T, sep = ',')
  finalData = rbind(finalData, loopFileData)
}

#summary of data
head(finalData)
summary(finalData)

#define a new categorical variable age_group and divide the data based on this variable
age_group <- cut(finalData$Age, breaks=c(0,18,24,34,44,54,64,100), labels=c("<18", "18-24", "25-34", "35-44", "44-54", "55-64", "65+"))
finalData$age_group <- cut(finalData$Age, breaks=c(0,18,24,34,44,54,64,100))
levels(age_group)

siterange <- function(x){c(length(x), min(x), mean(x), max(x))}
finalData$hasimps <-cut(finalData$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks~hasimps, data =finalData, FUN=siterange)
ggplot(subset(finalData, Clicks>0), aes(x=age_group, y=Clicks/Impressions, fill=age_group)) + geom_boxplot()

#define a new categorical variable click_behaviour and divide the data based on this variable
click_behaviour <- cut(finalData$Clicks, breaks=c(-Inf, 0, Inf), labels=c("Not-Clicked", "Clicked"))

#EDA based on user demographics
#Proportion of total females >18 yrs of age, Impressions>0 and whether they clicked or not
fem1 <- finalData[finalData$Gender == 0 & finalData$Age >18 & finalData$Impressions > 0, ]
click_behaviour1 <- cut(fem1$Clicks, breaks=c(-Inf, 0, Inf), labels=c("Not-Clicked", "Clicked"))
fem1_table <- table(click_behaviour1)
fem1_table
sum(fem1_table)
percentClicked1<- round((100*((fem1_table)/sum(fem1_table))), 1)
percentClicked1
barplot(percentClicked1, main="Proportion of females >18 Yrs and Impressions>0", xlab="Clicked vs Not-Clicked", ylab="%", las=1, names.arg=c("Not-Clicked", "Clicked"))

#Proportion of total females vs males <18 yrs of age, Signed_In=1 and whether they clicked or not
#This tells us the differences in behavioural patterns of males and females based how many of them signed_in as well as clicked
fem2 <- finalData[finalData$Gender == 0 & finalData$Age <18 & finalData$Signed_In == 1, ]
male2 <- finalData[finalData$Gender == 1 & finalData$Age <18 & finalData$Signed_In == 1, ]

click_behaviour2 <- cut(fem2$Clicks, breaks=c(-Inf, 0, Inf), labels=c("Not-Clicked", "Clicked"))
click_behaviour3 <- cut(male2$Clicks, breaks=c(-Inf, 0, Inf), labels=c("Not-Clicked", "Clicked"))
table_fem2 <- table(click_behaviour2)
table_fem2
table_male2 <- table(click_behaviour3)
table_male2

percentlabel1<- round((100*((table_fem2)/(sum(table_fem2)))), 1)
percentlabel2<- round((100*((table_male2)/(sum(table_male2)))), 1)
pielabel1<- paste(percentlabel1, "%", sep="")
pielabel2<- paste(percentlabel2, "%", sep="")

#Pie plots to distinguish between what proportion of males and females signed_in as well as clicked
pie(table_fem2, main="Proportion of females signed in and clicked(<18Yrs)", col=rainbow(length(table_fem2)), labels=pielabel1, cex=0.8)
legend("topright", c("Females Not-Clicked","Females Clicked"), cex=0.6, fill=rainbow(length(table_fem2)))

pie(table_male2, main="Proportion of males signed in and clicked(<18Yrs)", col=rainbow(length(table_male2)), labels=pielabel2, cex=0.8)
legend("topright", c("Males Not-Clicked","Males Clicked"), cex=0.6, fill=rainbow(length(table_male2)))

#summary and analysis of data
head(finalData)
summary(finalData)

signed_in_data1 <- finalData[finalData$Impressions > 0 & finalData$Clicks > 0, ]
signed_in_data1$signed_in_data2 <- cut(signed_in_data1$Signed_In, breaks=c(-Inf, 0, Inf), labels=c("Not_Signed_In", "Signed_In"))
summary(signed_in_data1)