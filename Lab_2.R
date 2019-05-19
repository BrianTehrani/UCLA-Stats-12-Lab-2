# Lab 2

##### Exercise 1 #####
# Set work directory
setwd("E:/Documents/School/UCLA/Stats 12/Week 3")

#a read in flint water data from csv file
flint <- read.csv(file = "flint.csv")

#b proportion of lead that is greater than 15 PPB
mean(flint$Pb >= 15)

#c mean Cu lvls for only Northern region
mean(flint$Cu[flint$Region == "North"])

#d mean Cu lvl for only dangerous conc.
mean(flint$Cu[flint$Danger_Cu == "Y"])

#e Pb observations above 0 & Below  Danger lvl
mean(flint$Pb < 15 & flint$Pb > 0)

#f Mean Pb and Cu lvls
mean(flint$Pb)
mean(flint$Cu)

#g Boxplot of Pb lvls
flint_Pb_North <- flint$Pb[flint$Region == "North"]
flint_Pb_South <- flint$Pb[flint$Region == "South"]
boxplot(flint_Pb_North,flint_Pb_South, main = "Flint Lead Levels", xlab = "Regions",
        ylab = "Lead level [PPB]", names = c("North", "South"))

#h
# The boxplot is difficult to see as many of the points sho up as outliers, Median better?

##### Exercise 2 #####
#a
life <- read.table("http://www.stat.ucla.edu/~nchristo/statistics12/countries_life.txt",
                   header=TRUE)
plot(life$Income, life$Life, ylab = "Life", xlab = "Income", main = "Country Life")
# When income increases, Life expectancy increases

#b
boxplot(life$Income, main = "Boxplot of Country Life", ylab = "Income", 
        xlab="101 Countries")
hist(life$Income,main = "Histogram of Country Life", xlab = "Income")
#Are there any outliers? => yes

#c
income_more_1000 <- life$Income[life$Income >= 1000]
income_less_1000 <- life$Income[life$Income < 1000]

#d plot Life against Income
life_less_1000 <- life$Life[life$Income < 1000]
plot(income_less_1000,life_less_1000, main = "Plot of Life Against Income Below $1000"
     , ylab = "Life Expectancy", xlab = "Income [$]")
cor(income_less_1000,life_less_1000)

##### Exercise 3 #####
#a
maas <- read.table("http://www.stat.ucla.edu/~nchristo/statistics12/soil.txt", header=TRUE)
summary(maas$lead)
summary(maas$zinc)
IQR(maas$zinc)

#b
par(mfcol=c(2,1))
hist(maas$lead) 
hist(log(maas$lead))

#c
#plot of logs of lead against zinc
plot(log(maas$zinc), log(maas$lead), main = "Log of Pb against Log of Zn")
# there is a correlation between the two variables

#d
risk_colors <- c("green", "orange", "red")
risk_lvl <- cut(maas$lead,c( 0, 149, 399, 100000))
as.numeric(risk_lvl)
plot(maas$lead, main = "Mean Concentration of Lead", ylab = "Lead Conc. in ppm",
     xlab = "Locations")
points(maas$lead, col=risk_colors[as.numeric(risk_lvl)], pch=9)

##### Exercise 4 #####
#a
LA <- read.table("http://www.stat.ucla.edu/~nchristo/statistics12/la_data.txt", header=TRUE)
library(maps)
plot(LA$Longitude, LA$Latitude, ylim = c(33.7, 34.35), xlim = c(-119, -118),
     xlab = "Longitude", ylab = "Latitude", main = "Centers within the City of LA")

map("county", "california", add = TRUE)

#b
LA_Schools_not_Zero <- LA$Income[LA$Schools != 0]
LA_Income_Schools_not_zero <- LA$Schools[LA$Schools !=0]
plot(LA_Schools_not_Zero, LA_Income_Schools_not_zero, main = "School Performance versus Income Excluding 0",
     xlab = "Income [$]", ylab = "School Performance")

#c
mean_LA_Income <- mean(LA$Income)
#sd_LA_Income <- sd(LA$Income)*sqrt((length(LA$Income)-1)/length(LA$Income))
sd_LA_Income = sd(LA$Income)
z <- (100000-mean_LA_Income)/ sd_LA_Income
z

#d
#Inc_prob <- pnorm(LA$Income, mean = mean_LA_Income, sd = sd_LA_Income , lower.tail = TRUE, log.p = FALSE)

total <- length(LA$Income)

# Function ... nSD is the number of SD you are looking at 
pData <- function(nSD){ 
  lo = mean_LA_Income - nSD/2*sd_LA_Income 
  hi = mean_LA_Income + nSD/2*sd_LA_Income 
  percent = sum(LA$Income>=lo & LA$Income<=hi)/total *100 
} 
print(paste("Percent of data within 1 SD is ",pData(1),"%", sep="")) 
print(paste("Percent of data within 2 SD is ",pData(2),"%", sep="")) 
print(paste("Percent of data within 3 SD is ",pData(3),"%", sep="")) 
