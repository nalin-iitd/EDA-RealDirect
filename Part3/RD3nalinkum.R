# textbook script for brookyln
#install the required packages
install.packages("gdata")
install.packages("data.table")

#load the required libraries
require(gdata)
require(data.table)

#read the brookyln excel file and load the dataset from single brooklyn file
bk <- read.xls("D:/DIC/dds_datasets/rollingsales_brooklyn.xls",pattern="BOROUGH", perl = "C:\\Perl64\\bin\\perl.exe")

#perform summary on the imported data set from single brookyln file 
head(bk)
summary(bk)

## clean/format the data with regular expressions
names(bk) <- tolower(names(bk))
names(bk)

bk$sale.price.n <- as.numeric(gsub("[^[:digit:]]","",
                                          bk$sale.price))
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                        bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",
                                       bk$land.square.feet))
bk$tax.class.at.time.of.sale <- as.numeric(gsub("[^[:digit:]]","",
                                                       bk$tax.class.at.time.of.sale))
bk$sale.date <- as.Date(bk$sale.date)
bk$year.built <- as.numeric(as.character(bk$year.built))
bk$address <- as.character(bk$address)
bk$block <- as.numeric(as.character(bk$block))
bk$lot <- as.numeric(as.character(bk$lot))
bk$zip.code <- as.numeric(as.character(bk$zip.code))

class(bk$sale.price.n)
class(bk$gross.sqft)
class(bk$land.sqft)

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bk)
hist(sale.price.n)
hist(sale.price.n[sale.price.n>0])
hist(gross.sqft[sale.price.n==0])
detach(bk)

## keep only the actual sales and plot between sale price and gross sq ft
bk.sale <- bk[bk$sale.price.n!=0,]
plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n))

## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("FAMILY",
                                bk.sale$building.class.category)),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))
bk.homes[which(bk.homes$sale.price.n<100000),]
order(bk.homes[which(bk.homes$sale.price.n<100000),]$sale.price.n)

## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))

#declare a new categorical variable apt_class and plot No. of Apartments vs Apt Class
bk$apt_class <- cut(bk$sale.price.n, breaks=c(-Inf, 0, 100000, 1000000, 5000000, Inf), labels=c("Zero", "Cheap", "Mid", "Costly", "Expensive"))
levels(bk$apt_class)
table(bk$apt_class)
barplot(table(bk$apt_class), main="No. of Apartments vs Apartment Sale Class", xlab="Apartment Sale Class", ylab="No. of Apartments", col=rainbow(5), ylim=c(0,12000), las=1)

#Plot Top 10 sales by neighborhood
bk$a <- bk$sale.price.n
bk$aa <- bk$neighborhood
aaa <- sqldf("SELECT a AS A, aa AS B FROM bk ORDER BY A DESC LIMIT 10")
class(aaa)
aaa$A <- aaa$A/1000000
barplot(aaa$A, main = "Top 10 Sales by Neighborhood", xlab="Neighborhoods", ylab="Top 10 Sale Prices(in million$)",ylim=c(0,300), las=2, col=rainbow(10), beside=TRUE, legend = c(aaa$B), args.legend = list(title = "Areas", x = "topright", cex = .5))

#Plot Top 10 sales by zip code
bk$b <- bk$sale.price.n
bk$bb <- bk$zip.code
bbb <- sqldf("SELECT b AS A, bb AS B FROM bk ORDER BY A DESC LIMIT 10")
class(bbb)
bbb$A <- bbb$A/1000000
barplot(bbb$A, main = "Top 10 Sales by Zip Code", xlab="Zip Code", ylab="Top 10 Sale Prices(in million$)",ylim=c(0,300), las=2, col=rainbow(10), beside=TRUE, legend = c(bbb$B), args.legend = list(title = "Zip", x = "topright", cex = .5))

#Plot Top 10 sales by tax class at time of sale
bk$c <- bk$sale.price.n
bk$cc <- bk$tax.class.at.time.of.sale
ccc <- sqldf("SELECT c AS A, cc AS B FROM bk ORDER BY A DESC LIMIT 10")
class(ccc)
ccc$A <- ccc$A/1000000
barplot(ccc$A, main = "Top 10 Sales by Tax Class", xlab="Tax Class", ylab="Top 10 Sale Prices(in million$)",ylim=c(0,300), las=2, col=rainbow(10), beside=TRUE, legend = c(ccc$B), args.legend = list(title = "Class", x = "topright", cex = .5))

#define a new categorical variable for year built and plot No. of Apartments Sold vs Year Built
bk$apt_year_built <- cut(bk$year.built, breaks=c(-Inf,1900,1925,1950,1975,2000,Inf), labels=c("<1900", "1900-1925", "1925-1950", "1950-1975", "1975-2000", ">2000"))
levels(bk$apt_year_built)
table(bk$apt_year_built)
barplot(table(bk$apt_year_built), main="No. of Apartments Sold vs Year Built", ylab="No. of Apartments", col=rainbow(7), ylim=c(0,30000), las=2)




