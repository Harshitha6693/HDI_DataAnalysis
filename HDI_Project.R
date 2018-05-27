install.packages("RCurl")
library(RCurl)
install.packages("XML")
library(XML)

#  Getting URL data from the website specified in the getURL function.
webpage <- getURL("https://en.wikipedia.org/wiki/List_of_United_States_counties_by_per_capita_income") # gets the URL
# Collecting the data from the tables in the web page and storing in data frames.
table1 <- readHTMLTable(webpage, which = 2)
table2 <- readHTMLTable(webpage, which = 3) 
table3 <- readHTMLTable(webpage, which = 4)
table4 <- readHTMLTable(webpage, which = 5)
table5 <- readHTMLTable(webpage, which = 6)
table6 <- readHTMLTable(webpage, which = 7)
table7 <- readHTMLTable(webpage, which = 8)
table8 <- readHTMLTable(webpage, which = 9)
table9 <- readHTMLTable(webpage, which = 10)
table10 <- readHTMLTable(webpage, which = 11)
   
# Combining all the individual data frames into a single one using rbind.
HDI <- rbind(table1,table2, table3, table4, table5, table6, table7, table8, table8, table9, table10)
nrow(HDI)

# Few rows had some inappropriate data with the word State and Country under the State column. So these were removed.
ind <- which(HDI$State == "State")
ind
HDI <- HDI[-ind,]
ind1 <- which(HDI$State == "Country")
ind1
HDI <- HDI[-ind1,]

# changing the names of columns
colnames(HDI) <- c("Rank","County-equivalent","State","Per_capita_income","Median_household_income","Median_family_income","Population","Number_of_households")
HDI
class(HDI$Per_capita_income)

# Removing the special characters "," and "$" and converting into numeric type.
HDI$Per_capita_income <- gsub(",","",HDI$Per_capita_income)
HDI$Per_capita_income <- as.numeric(gsub("$","", HDI$Per_capita_income,fixed = TRUE))

HDI$Median_household_income <- gsub(",","",HDI$Median_household_income)
HDI$Median_household_income <- as.numeric(gsub("$","", HDI$Median_household_income,fixed = TRUE))

HDI$Median_family_income <- gsub(",","",HDI$Median_family_income)
HDI$Median_family_income <- as.numeric(gsub("$","", HDI$Median_family_income,fixed = TRUE))

HDI$Population <- as.numeric(gsub(",","",HDI$Population))

HDI$Number_of_households <- as.numeric(gsub(",","",HDI$Number_of_households))
HDI

levels(HDI$State)

# Function that returns the city/county with highest median household_income for each state
highest_median_household_income_each_state <- function(states)
{
    as.character(states)
    s1 <- HDI[which(HDI$State == states),]
    highest <- s1[which.max(s1$Median_household_income),]
    message("The city ",highest[,2], " has the highest median household income of $",highest[,5]," for the state ",highest[,3])
    
}

for (states in levels(HDI$State))
{
    highest_median_household_income_each_state(states) 
}

# Function that returns the city/county with highest median family_income for each state
highest_median_family_income_each_state <- function(states)
{
    as.character(states)
    s1 <- HDI[which(HDI$State == states),]
    highest <- s1[which.max(s1$Median_family_income),]
    message("The city ",highest[,2], " has the highest median family income of $",highest[,6]," for the state ",highest[,3])
    
}

for (states in levels(HDI$State))
{
    highest_median_family_income_each_state(states) 
}

# Function that returns the city/county with highest per capita income for each state
highest_per_capita_income_each_state <- function(states)
{
    as.character(states)
    s1 <- HDI[which(HDI$State == states),]
    highest <- s1[which.max(s1$Per_capita_income),]
    message("The city ",highest[,2], " has the highest per_capita_income of $",highest[,4]," for the state ",highest[,3])
    
}

for (states in levels(HDI$State))
{
    highest_per_capita_income_each_state(states) 
}

# Finding the top 50 cities with highest per capita income
toppercapitaincome <- HDI[order(HDI$Per_capita_income,decreasing = TRUE),]
top50percapitaincome <- toppercapitaincome[1:50,]
top50percapitaincome 

# Finding the top 50 cities with highest median household income
topmedianhousehold <- HDI[order(HDI$Median_household_income,decreasing = TRUE),]
top50medianhousehold <- topmedianhousehold[1:50,]
top50medianhousehold

# Finding the top 50 cities with highest median family income
topmedianfamily <- HDI[order(HDI$Median_family_income,decreasing = TRUE),]
top50medianfamily <- topmedianfamily[1:50,]
top50medianfamily

# Finding the top 50 cities with highest population
toppopulation <- HDI[order(HDI$Population,decreasing = TRUE),]
top50population <- toppopulation[1:50,]
top50population



avgpercapitaincome <- c()
avgpercapitaincomestate <- c()
c <- 0
# Function that finds average per capita income for each state

averagepercapitaincome <- function(states, c)
{
    as.character(states)
    s1 <- HDI[which(HDI$State == states),]
    average <- mean(s1$Per_capita_income,na.rm = TRUE)
    message("The state ", states," has an average per capita income of ", average)
    avgpercapitaincome[c] <<- average
    avgpercapitaincomestate[c] <<- states
  
}


for (states in levels(HDI$State))
{
    c <- c + 1
    averagepercapitaincome(states,c) 
}

avgmedianhouseholdincome <- c()
avgmedianhouseholdincomestate <- c()
c <- 0

# Function that finds average median household income for each state

averagehouseholdincome <- function(states, c)
{
    as.character(states)
    s1 <- HDI[which(HDI$State == states),]
    average <- mean(s1$Median_household_income,na.rm = TRUE)
    message("The state ", states," has an average median household income of ", average)
    avgmedianhouseholdincome[c] <<- average
    avgmedianhouseholdincomestate[c] <<- states
    
}


for (states in levels(HDI$State))
{
    c <- c + 1
    averagehouseholdincome(states,c) 
}


avgmedianfamilyincome <- c()
avgmedianfamilyincomestate <- c()
c <- 0

# Function that finds average median family income for each state

averagefamilyincome <- function(states, c)
{
    as.character(states)
    s1 <- HDI[which(HDI$State == states),]
    average <- mean(s1$Median_family_income,na.rm = TRUE)
    message("The state ", states," has an average median family income of ", average)
    avgmedianfamilyincome[c] <<- average
    avgmedianfamilyincomestate[c] <<- states
    
}


for (states in levels(HDI$State))
{
    c <- c + 1
    averagefamilyincome(states,c) 
}

sumpopulation <- c()
sumpopulationstate <- c()
c <- 0

# Function that finds average population for each state

averagepopulation <- function(states, c)
{
    as.character(states)
    s1 <- HDI[which(HDI$State == states),]
    total <- sum(s1$Population,na.rm = TRUE)
    message("The state ", states," has an average population of ", total)
    sumpopulation[c] <<- total
    sumpopulationstate[c] <<- states
    
}


for (states in levels(HDI$State))
{
    c <- c + 1
    averagepopulation(states,c) 
}

percapitaincomeavg <- data.frame(avgpercapitaincomestate,avgpercapitaincome)
householdincomeavg <- data.frame(avgmedianhouseholdincomestate,avgmedianhouseholdincome)
familyincomeavg <- data.frame(avgmedianfamilyincomestate,avgmedianfamilyincome)
populationsum<- data.frame(sumpopulationstate,sumpopulation)




percapitaincomeavg <- percapitaincomeavg[-c(40,51),]
householdincomeavg <- householdincomeavg[-c(40,51),]
familyincomeavg <- familyincomeavg[-c(40,51),]
populationsum <- populationsum[-c(40,51),]

# Plotting average per capita income, average median household income, average median family income and total
# population for all states
barplot(percapitaincomeavg$avgpercapitaincome, main="Average per capita income by states", xlab="State",  
        ylab="Average Per Capita Income", names.arg=percapitaincomeavg$avgpercapitaincomestate, 
        border="red")


barplot(familyincomeavg$avgmedianfamilyincome, main="Average median family income by states", xlab="State",  
        ylab="Average Median Family Income", names.arg=familyincomeavg$avgmedianfamilyincomestate, 
        border="green")

barplot(familyincomeavg$avgmedianfamilyincome, main="Average median family income by states", xlab="State",  
        ylab="Average Median Family Income", names.arg=familyincomeavg$avgmedianfamilyincomestate, 
        border="green")

barplot(householdincomeavg$avgmedianhouseholdincome, main="Average median household income by states", xlab="State",  
        ylab="Average Median Household Income", names.arg=householdincomeavg$avgmedianhouseholdincomestate, 
        border="violet")


barplot(populationsum$sumpopulation, main="Population by States", xlab="State",  
        ylab="Population", names.arg=populationsum$sumpopulationstate, 
        border="blue")


install.packages("RSQLite")
library(RSQLite)
install.packages("dplyr")
library(dplyr)


# creating a databse HDI_db and storing the data into the
HDI_db <- src_sqlite("HDI_db.sqlite3", create = TRUE)
HDI_sqlite <- copy_to(HDI_db,HDI,temporary = FALSE)

# A qyery is used ti comunicate to the database to check whether the database connection is working
tbl(HDI_db,sql("SELECT `County-equivalent`, State, Per_capita_income FROM HDI WHERE Per_capita_income > 50000 "))


