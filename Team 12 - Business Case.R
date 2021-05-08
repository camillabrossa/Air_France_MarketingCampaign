################################################################
################################################################
##############   AIR FRANCE BUSINESS CASE   ####################
################################################################
################################################################ 

## Authors: Team 12 - MSBA 1
# Camilla Brossa
# Taylor Julian
# Pedro Battistelli
# Jan Grau

# Checklist !!!!!
# confusion matrix, training-testing, tree

#Installing packages
install.packages("plotly")
install.packages("mlbench")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ngram")
install.packages("Amelia")
install.packages("scales")

# Calling packages
library(readxl)
library(plotly)
library(ggplot2)
library(mlbench)
library(rpart)
library(rpart.plot)
library(ngram)
library(Amelia)
library(scales)


# Importing database
airfrance <- read_excel("Desktop/HULT Business School/MBAN/R/Air France Case Spreadsheet Supplement (2).xls")
View(airfrance)


#######################################
## Data Cleaning & Massaging
#######################################

# Checking structure of our dataframe
str(airfrance)

# Update NAs
airfrance[airfrance==""] <- NA
airfrance[airfrance=="N/A"] <- NA

# Visualizing graphically missing data
missmap(airfrance, col=c("blue", "red"), legend = TRUE)

# Checking the amount of NA
sapply(airfrance, function(x) sum(is.na(x)))

# Since it's just 1% of data and only in 2 columns, we would sacrifice too much data
# potentially useful for our analysis

# We will then drop the Bid Strategy column because it's the only one that has 
# many NA values (1224), while we keep the remaining variables (also Match Type, that had 48 NA)
dropped_airfrance <- airfrance[,-9]

#######################################
## Data Filtering and Subsetting
#######################################

# Filtering the dataset to get only values regarding the US (Search Engine = US)
dropped_airfrance$US_search_engines <- dropped_airfrance$'Publisher Name'
dropped_airfrance$US_search_engines <- gsub("..*US", 1, dropped_airfrance$US_search_engines)
sub_df <- dropped_airfrance[which(dropped_airfrance$US_search_engines == 1), ]

#Transforming Match Type to a factor
sub_df$`Match Type` <- factor(sub_df$`Match Type`, 
                                       ordered = T,
                                       levels = c("Broad", "Standard", "Exact", "Advanced"))


######################################################
######  Creating KPIs
######################################################

# Profit (aka Net Revenues) = Total Revenues ('Amount') - Total Cost 
sub_df$'Profit'<- as.numeric(sub_df$'Amount' - sub_df$'Total Cost')

# ROA = Profit / Total Cost
sub_df$'ROA' <- as.numeric(sub_df$'Profit' / sub_df$'Total Cost')

# Average revenue per booking = Total Volume of Bookings / Amount
sub_df$'Avg Revenue/booking' <- as.numeric(sub_df$'Total Volume of Bookings' / sub_df$'Amount')

# Probability of Booking = (Trans. Conv. % * Engine Click Thru %)/10000
sub_df$'Probability of Booking' <- label_percent(digits = 2)((sub_df$'Trans. Conv. %' * sub_df$'Engine Click Thru %')/10000)


#################################################################################
## Code for Question 2 
## How can campaigns be improved to increase overall value gained from 
## investment with a search engine publisher? Should keywords be added or 
## dropped from the campaign? Should campaign tactics or copy be adjusted to 
## improve campaign performance?
#################################################################################


###### CREATING SMALLER DATAFRAMES TO ANALYZE THE PERFORMANCE OF EACH SEARCH ENGINE
###### AND EACH CAMPAIGN PER KEYWORD MATCH TYPE, BY USING THE KPIs


#####################################################
##### CREATING THE SUBSET FOR EACH SEARCH ENGINE
####################################################

# Analyzing each search engine's performance based on their average Profit, ROA,
# Average revenue per Booking and Probability of Booking, per Keyword Match Type

# Create a vector with match types names in order to name the columns of each dataframe
match_types <- c("Broad", "Standard", "Exact", "Advanced") 

# PROFIT
# Create empty vectors to store average profit per eache search engine
google_US_profit <- c()
msn_US_profit <- c()
overture_US_profit <- c()
yahoo_US_profit <- c()
i <- 1 #--> setting i to start at 1

# While loop to get average net revenue per Publisher, per match type
while(i <= length(match_types)){
  
  google_US_profit <- c(google_US_profit, mean(sub_df$'Profit'[
    which(sub_df$`Publisher Name` == "Google - US" & 
            sub_df$`Match Type` == match_types[i])],na.rm=TRUE))
  
  msn_US_profit <- c(msn_US_profit, mean(sub_df$'Profit'[
    which(sub_df$`Publisher Name` == "MSN - US" & 
            sub_df$`Match Type` == match_types[i])],na.rm=TRUE))
  
  overture_US_profit <- c(overture_US_profit, mean(sub_df$'Profit'[
    which(sub_df$`Publisher Name` == "Overture - US" & 
            sub_df$`Match Type` == match_types[i])],na.rm=TRUE))
  
  yahoo_US_profit <- c(yahoo_US_profit, mean(sub_df$'Profit'[
    which(sub_df$`Publisher Name` == "Yahoo - US" & 
            sub_df$`Match Type` == match_types[i])],na.rm=TRUE))
  
  i <- i + 1
  
} #--> closing while loop

# Combine average ROA per publisher in a matrix
profit_search_engines <- cbind(  google_US_profit, msn_US_profit , overture_US_profit, 
                                yahoo_US_profit)

# naming the rows
rownames(profit_search_engines) <- match_types  

print(profit_search_engines)


# ROA
# Create empty vectors to store ROA per search engine
google_US_ROA <- c()
msn_US_ROA <- c()
overture_US_ROA <- c()
yahoo_US_ROA <- c()
i <- 1 #--> setting i to start at 1

# While loop to get average ROA per Publisher, per match type
while(i <= length(match_types)){
  
  google_US_ROA <- c(google_US_ROA, mean(sub_df$ROA[
    which(sub_df$`Publisher Name` == "Google - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  msn_US_ROA <- c(msn_US_ROA, mean(sub_df$ROA[
    which(sub_df$`Publisher Name` == "MSN - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  overture_US_ROA <- c(overture_US_ROA, mean(sub_df$ROA[
    which(sub_df$`Publisher Name` == "Overture - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  yahoo_US_ROA <- c(yahoo_US_ROA, mean(sub_df$ROA[
    which(sub_df$`Publisher Name` == "Yahoo - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  i <- i + 1
  
} #--> closing while loop

# Combine average ROA per publisher in a matrix
ROA_search_engines <- cbind(google_US_ROA, msn_US_ROA, 
                           overture_US_ROA, yahoo_US_ROA)

rownames(ROA_search_engines) <- match_types 

print(ROA_search_engines)


# AVERAGE REVENUE PER BOOKING 
# Create empty vectors to store Avg Revenue/booking per search engine
google_US_revperbook <- c()
msn_US_revperbook <- c()
overture_US_revperbook <- c()
yahoo_US_revperbook <- c()
i <- 1 #--> setting i to start at 1

# While loop to get average ROA per Publisher, per match type
while(i <= length(match_types)){
  
  google_US_revperbook <- c(google_US_revperbook, mean(sub_df$'Avg Revenue/booking'[
    which(sub_df$`Publisher Name` == "Google - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  msn_US_revperbook <- c(msn_US_revperbook, mean(sub_df$'Avg Revenue/booking'[
    which(sub_df$`Publisher Name` == "MSN - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  overture_US_revperbook <- c(overture_US_revperbook, mean(sub_df$'Avg Revenue/booking'[
    which(sub_df$`Publisher Name` == "Overture - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  yahoo_US_revperbook <- c(yahoo_US_revperbook, mean(sub_df$'Avg Revenue/booking'[
    which(sub_df$`Publisher Name` == "Yahoo - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  i <- i + 1
  
} #--> closing while loop

# Combine average Revenues per Booking in a matrix
revperbook_search_engines <- cbind(google_US_revperbook, msn_US_revperbook, 
                            overture_US_revperbook, yahoo_US_revperbook)

rownames(revperbook_search_engines) <- match_types #--> assign names to the rows

print(revperbook_search_engines)

# PROBABILITY OF BOOKING 
# Create empty vectors to store Probability of Booking per search engine
google_US_prob_booking <- c()
msn_US_prob_booking <- c()
overture_US_prob_booking <- c()
yahoo_US_prob_booking <- c()
i <- 1 #--> setting i to start at 1

# While loop to get average ROA per Publisher, per match type
while(i <= length(match_types)){
  
  google_US_prob_booking <- c(google_US_prob_booking, mean(sub_df$'Probability of Booking'[
    which(sub_df$`Publisher Name` == "Google - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  msn_US_prob_booking <- c(msn_US_prob_booking, mean(sub_df$'Probability of Booking'[
    which(sub_df$`Publisher Name` == "MSN - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  overture_US_prob_booking <- c(overture_US_prob_booking, mean(sub_df$'Probability of Booking'[
    which(sub_df$`Publisher Name` == "Overture - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  yahoo_US_prob_booking <- c(yahoo_US_prob_booking, mean(sub_df$'Probability of Booking'[
    which(sub_df$`Publisher Name` == "Yahoo - US" & 
            sub_df$`Match Type` == match_types[i])]))
  
  i <- i + 1
  
} #--> closing while loop

# Combine average Revenues per Booking in a matrix
prob_booking_search_engines <- cbind(google_US_prob_booking, msn_US_prob_booking, 
                                   overture_US_prob_booking, yahoo_US_prob_booking)

rownames(prob_booking_search_engines) <- match_types #--> assign names to the rows

print(prob_booking_search_engines)

#####################################################
##### CREATING THE SUBSET FOR CAMPAIGNS
####################################################

table(sub_df$Campaign)

Air_France_Brand_French_Destinations_ROA_avg <- c()                   
Air_France_Branded_ROA_avg <- c()             
Air_France_Global_Campaign_ROA_avg <- c() 
Business_Class_ROA_avg <- c()                    
French_Destinations_ROA_avg <- c()                          
General_Terms_ROA_avg <- c() 
Geo_Targeted_Boston_ROA_avg <- c()                   
Geo_Targeted_Chicago_ROA_avg <- c()                        
Geo_Targeted_DC_ROA_avg <- c() 
Geo_Targeted_Detroit_ROA_avg <- c()                   
Geo_Targeted_Houston_ROA_avg <- c()               
Geo_Targeted_Los_Angeles_ROA_avg <- c() 
Geo_Targeted_Miami_ROA_avg <- c()                  
Geo_Targeted_New_York_ROA_avg <- c()              
Geo_Targeted_Philadelphia_ROA_avg <- c() 
Geo_Targeted_San_Francisco_ROA_avg <- c()                   
Geo_Targeted_Seattle_ROA_avg <- c()                   
Google_Yearlong_ROA_avg <- c()
Outside_Western_Europe_ROA_avg <- c()                   
Paris_France_Terms_ROA_avg <- c()                             
Unassigned_ROA_avg <- c() 
Western_Europe_Destinations_ROA_avg <- c() 
i <- 1

while(i <= length(match_types)){
  
  Air_France_Brand_French_Destinations_ROA_avg <- 
    c(Air_France_Brand_French_Destinations_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Air France Brand & French Destinations" 
            & sub_df$`Match Type` == match_types[i])]))   
  
  Air_France_Branded_ROA_avg <- 
    c( Air_France_Branded_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Air France Branded" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Air_France_Global_Campaign_ROA_avg <- 
    c( Air_France_Global_Campaign_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Air France Global Campaign" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Business_Class_ROA_avg <- 
    c( Business_Class_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Business Class" 
            & sub_df$`Match Type` == match_types[i])]))
  
  French_Destinations_ROA_avg <- 
    c( French_Destinations_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "French Destinations" 
            & sub_df$`Match Type` == match_types[i])]))
  
  General_Terms_ROA_avg <- 
    c( General_Terms_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "General Terms" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_Boston_ROA_avg <- 
    c( Geo_Targeted_Boston_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted Boston" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_Chicago_ROA_avg <- 
    c( Geo_Targeted_Chicago_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted Chicago" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_DC_ROA_avg <- 
    c( Geo_Targeted_DC_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted DC" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_Detroit_ROA_avg <- 
    c( Geo_Targeted_Detroit_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted Detroit" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_Houston_ROA_avg <- 
    c( Geo_Targeted_Houston_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted Houston" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_Los_Angeles_ROA_avg <- 
    c( Geo_Targeted_Los_Angeles_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted Los Angeles" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_Miami_ROA_avg <- 
    c( Geo_Targeted_Miami_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted Miami" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_New_York_ROA_avg <- 
    c( Geo_Targeted_New_York_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted New York" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_Philadelphia_ROA_avg <- 
    c( Geo_Targeted_Philadelphia_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted Philadelphia" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_San_Francisco_ROA_avg <- 
    c( Geo_Targeted_San_Francisco_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted San Francisco" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Geo_Targeted_Seattle_ROA_avg <- 
    c( Geo_Targeted_Seattle_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Geo Targeted Seattle" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Google_Yearlong_ROA_avg <- 
    c( Google_Yearlong_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Google_Yearlong" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Outside_Western_Europe_ROA_avg <- 
    c( Outside_Western_Europe_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Outside Western Europe" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Paris_France_Terms_ROA_avg <- 
    c( Paris_France_Terms_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Paris & France Terms" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Unassigned_ROA_avg <- 
    c( Unassigned_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Unassigned" 
            & sub_df$`Match Type` == match_types[i])]))
  
  Western_Europe_Destinations_ROA_avg <- 
    c( Western_Europe_Destinations_ROA_avg, mean(sub_df$ROA[
      which(sub_df$Campaign == "Western Europe Destinations" 
            & sub_df$`Match Type` == match_types[i])]))
  i <- i + 1
} #--> closing while loop

# Combine average ROA per publisher in a matrix
campaign_ROA_per_match_type <- rbind(Air_France_Brand_French_Destinations_ROA_avg, 
                                     Air_France_Branded_ROA_avg, 
                                     Air_France_Global_Campaign_ROA_avg, 
                                     Business_Class_ROA_avg, 
                                     French_Destinations_ROA_avg, 
                                     General_Terms_ROA_avg, 
                                     Geo_Targeted_Boston_ROA_avg, 
                                     Geo_Targeted_Chicago_ROA_avg, 
                                     Geo_Targeted_DC_ROA_avg, 
                                     Geo_Targeted_Detroit_ROA_avg, 
                                     Geo_Targeted_Houston_ROA_avg, 
                                     Geo_Targeted_Los_Angeles_ROA_avg, 
                                     Geo_Targeted_Miami_ROA_avg, 
                                     Geo_Targeted_New_York_ROA_avg, 
                                     Geo_Targeted_Philadelphia_ROA_avg, 
                                     Geo_Targeted_San_Francisco_ROA_avg, 
                                     Geo_Targeted_Seattle_ROA_avg, 
                                     Google_Yearlong_ROA_avg, 
                                     Outside_Western_Europe_ROA_avg, 
                                     Paris_France_Terms_ROA_avg, 
                                     Unassigned_ROA_avg, 
                                     Western_Europe_Destinations_ROA_avg)

colnames(campaign_ROA_per_match_type) <- match_types  #--> assign names to the rows
print(campaign_ROA_per_match_type)


#################################################################################
## PERFORMANCE ANALYSIS
#################################################################################

# Dataframe sorted by HIGHEST PROFIT (taking the 10 best performant)

sorted_profit <- head(sub_df[order(sub_df$Profit, decreasing = TRUE), c(2,5,6,7,24)], n=10)
sorted_profit

# Dataframe sorted by HIGHEST ROA (taking the 10 best performant)

sorted_ROA <- head(sub_df[order(sub_df$ROA, decreasing = TRUE), c(2,5,6,7,25)], n=11)
sorted_ROA

# Dataframe sorted by HIGHEST PROBABILITY OF BOOKING (taking the 10 best performant)

sorted_prob <- head(sub_df[order(sub_df$'Probability of Booking', decreasing = TRUE), c(2,5,6,7,27)], n=10)
sorted_prob




#########
## PLOT OF THE BEST 10 performances
#########




##### LINEAR REGRESSION TO TEST THE RELATIONSHIP BETWEEN THE VARIABLES

## Dependent variable: Profit - What influences the profit (net revenue)?

# creating a new df where we put only the relevant variables and we change their names
# to make them callable in the linear regression 
linear_df <- sub_df[ ,c("Match Type", "Search Engine Bid", "Clicks", 
                       "Impressions", "Avg. Pos.", "Profit", "ROA", "Probability of Booking")]
names <- c("match_type","search_engine_bid", "clicks", 
           "impressions", "avg_pos", "profit", "ROA", "probability_of_booking" )
colnames(linear_df) <- names
as.numeric(linear_df$match_type)

linear <- lm(profit ~ match_type + search_engine_bid + clicks + impressions +
               avg_pos, data=linear_df)
summary(linear)

# re-running the regression after having removed insignificant variables (search engine bid and avg position)
linear_opt <- lm(profit ~ match_type + clicks + impressions, data=linear_df)
summary(linear_opt)                  


# Coefficient interpretation 
# Match type.L
exp(1.795)-1
# Match type.Q
exp(0.926)-1
# Match type.C
exp(0)-1
# Clicks
exp(1.319)-1
# Impressions
exp(0.00862)-1

# Test of heteroscedasticity (Breusch - Pagan Test) to check if the
# linear regression can be run
library(lmtest)
bptest(linear)

#########################################################
##### Testing Kayak 
#########################################################

# Data about Kayak
clicks_kayak <- 2839
media_cost_kayak <- 3567.13
tot_booking_kayak <- 208
avg_ticket_kayak <- 1123.53
revenue_kayak <- 233694
profit_kayak <- 230126.87

# Compare the data from kayak with the averages of the dataset
avg_clicks <- mean(sub_df$Clicks, na.rm=T)
avg_cost <- mean(sub_df$'Total Cost', na.rm=T)
avg_bookings <- mean(sub_df$'Total Volume of Bookings', na.rm=T)
avg_revenue <- mean(sub_df$'Amount', na.rm=T)
avg_profit <- mean(sub_df$'Profit', na.rm=T)

# Create a table where we compare the 2 
kayak_data <- c(clicks_kayak, media_cost_kayak, tot_booking_kayak,
              revenue_kayak, profit_kayak)
current_airfrance <- c(avg_clicks, avg_cost, avg_bookings,
                       avg_revenue, avg_profit)

names_col <- c("Clicks", "Avg.Cost", "Avg. Bookings", "Avg. Revenue",
                "Avg. Profit (Net Revenue)")
names_rows <- c("Current Performance", "Kayak")

compar_table <- rbind(current_airfrance, kayak_data)
rownames(compar_table) <- names_rows
colnames(compar_table) <- names_col

compar_table






