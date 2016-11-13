#install the required packages
install.packages("gdata")
install.packages("data.table")
install.packages("sqldf")

#load the required libraries
require(gdata)
require(data.table)
require(sqldf)

#script to load all 5 data sets
fileDir <- 'D:/dic_data/problem3/'
fileDir1 <- 'D:/dic_data/problem3/rollingsales_brooklyn.xls'
files <- list.files(path = fileDir, pattern = "\\.xls$")
finalData <- read.xls(fileDir1,pattern="BOROUGH", perl = "C:\\Perl64\\bin\\perl.exe")


for(i in 2:5){
  tempDir = paste(fileDir,files[i],sep = "")
  loopFileData <- read.xls(tempDir,pattern="BOROUGH", perl = "C:\\Perl64\\bin\\perl.exe")
  finalData = rbind(finalData, loopFileData)
}

head(finalData)
summary(finalData)
finalData[1:5, ]
finalData[104076:104080, ]
length(finalData)

# clean up the data
names(finalData) <- tolower(names(finalData))
names(finalData)
class(finalData$neighborhood)
class(finalData$block)
class(finalData$land.square.feet)
class(finalData$gross.square.feet)
class(finalData$sale.price)
class(finalData$sale.date)
class(finalData$zip.code)
class(finalData$address)
class(finalData$address)
class(finalData$tax.class.at.time.of.sale)
finalData$sale.price.n <- as.numeric(gsub("[^[:digit:]]","",
                                          finalData$sale.price))
finalData$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                        finalData$gross.square.feet))
finalData$land.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                       finalData$land.square.feet))
finalData$tax.class.at.time.of.sale <- as.numeric(gsub("[^[:digit:]]","",
                                          finalData$tax.class.at.time.of.sale))
finalData$sale.date <- as.Date(finalData$sale.date)
finalData$year.built <- as.numeric(as.character(finalData$year.built))
finalData$address <- as.character(finalData$address)
finalData$block <- as.numeric(as.character(finalData$block))
finalData$lot <- as.numeric(as.character(finalData$lot))
finalData$zip.code <- as.numeric(as.character(finalData$zip.code))

class(finalData$sale.price.n)
class(finalData$gross.sqft)
class(finalData$land.sqft)

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(finalData)
hist(sale.price.n)
hist(sale.price.n[sale.price.n>0])
hist(gross.sqft[sale.price.n==0])
detach(finalData)

## keep only the actual sales and plot between sale price and gross sq ft
finalData.sale <- finalData[finalData$sale.price.n!=0,]
plot(finalData.sale$gross.sqft,finalData.sale$sale.price.n)
plot(log(finalData.sale$gross.sqft),log(finalData.sale$sale.price.n))

## for now, let's look at 1-, 2-, and 3-family homes
finalData.homes <- finalData.sale[which(grepl("FAMILY",
                                finalData.sale$building.class.category)),]
plot(log(finalData.homes$gross.sqft),log(finalData.homes$sale.price.n))
finalData.homes[which(finalData.homes$sale.price.n<100000),]
order(finalData.homes[which(finalData.homes$sale.price.n<100000),]$sale.price.n)

## remove outliers that seem like they weren't actual sales
finalData.homes$outliers <- (log(finalData.homes$sale.price.n) <=5) + 0
finalData.homes <- finalData.homes[which(finalData.homes$outliers==0),]
plot(log(finalData.homes$gross.sqft),log(finalData.homes$sale.price.n))

#declare a new categorical variable apt_class and plot No. of Apartments vs Apt Class
finalData$apt_class <- cut(finalData$sale.price.n, breaks=c(-Inf, 0, 100000, 1000000, 5000000, Inf), labels=c("Zero", "Cheap", "Mid", "Costly", "Expensive"))
levels(finalData$apt_class)
table(finalData$apt_class)
barplot(table(finalData$apt_class), main="No. of Apartments vs Apartment Sale Class", xlab="Apartment Sale Class", ylab="No. of Apartments", col=rainbow(5), ylim=c(0,50000), las=1, beside=TRUE, legend = c(levels(finalData$apt_class)), args.legend = list(title = "Areas", x = "topright", cex = .5)
)

#Plot Top 10 sales by neighborhood
finalData$a <- finalData$sale.price.n
finalData$aa <- finalData$neighborhood
aaa <- sqldf("SELECT a AS A, aa AS B FROM finalData ORDER BY A DESC LIMIT 10")
class(aaa)
aaa$A <- aaa$A/1000000
barplot(aaa$A, main = "Top 10 Sales by Neighborhood", xlab="Neighborhoods", ylab="Top 10 Sale Prices(in million$)",ylim=c(0,1500), las=2, col=rainbow(10), beside=TRUE, legend = c(aaa$B), args.legend = list(title = "Areas", x = "topright", cex = .5))

#Plot Top 10 sales by zip code
finalData$c <- finalData$sale.price.n
finalData$cc <- finalData$zip.code
ccc <- sqldf("SELECT c AS A, cc AS B FROM finalData ORDER BY A DESC LIMIT 10")
class(ccc)
ccc$A <- ccc$A/1000000
barplot(ccc$A, main = "Top 10 Sales by Zipcode", xlab="Zipcode", ylab="Top 10 Sale Prices(in million$)",ylim=c(0,1500), las=2, col=rainbow(10), beside=TRUE, legend = c(ccc$B), args.legend = list(title = "Zip", x = "topright", cex = .5))

#Plot Top 10 sales by tax class at time of sale
finalData$e <- finalData$sale.price.n
finalData$ee <- finalData$tax.class.at.time.of.sale
eee <- sqldf("SELECT e AS A, ee AS B FROM finalData ORDER BY A DESC LIMIT 10")
class(eee)
eee$A <- eee$A/1000000
barplot(eee$A, main = "Top 10 Cumulative Sales by Tax Class", xlab="Tax Class", ylab="Top 10 CumulativeSale Prices(in million$)",ylim=c(0,1500), las=2, col=rainbow(10), beside=TRUE, legend = c(eee$B), args.legend = list(title = "Class", x = "topright", cex = .5))

#define a new categorical variable for year built and plot No. of Apartments Sold vs Year Built
finalData$apt_year_built <- cut(finalData$year.built, breaks=c(-Inf,1900,1925,1950,1975,2000,Inf), labels=c("<1900", "1900-1925", "1925-1950", "1950-1975", "1975-2000", ">2000"))
levels(finalData$apt_year_built)
table(finalData$apt_year_built)
barplot(table(finalData$apt_year_built), main="No. of Apartments Sold vs Year Built", ylab="No. of Apartments", col=rainbow(7), ylim=c(0,30000), las=2, beside=TRUE, legend = c(levels(finalData$apt_year_built)), args.legend = list(title = "year", x = "topright", cex = .5))
