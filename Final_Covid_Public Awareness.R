#####################################################################################################
#################Part I Bi-variate relationship between Coronavirus and Partisanship#################
#####################################################################################################

library(gtrendsR)
library(ggplot2)
library(haven)

# Step 1 - Choose the time period to compare

# Before March: Well before the outbreak. This is to provide a baseline showing that prior to the outbreak, there was no relationship between the partisanship of media markets and searches for coronavirus.
# March 1st - 12th: This was a period during which the oubreak was becoming a major problem but during which Republicans were frequently downplaying the issue.
# March 13th - Apr 12: President Trumnp declared a national emergency on March 13th. After this point, Republicans largely changed their attitude on the crisis, taking is more seriously than they had been previously.

# Step 2 - Use gtrends command to automatically pull google trends search data for three different time periods
coronavirus <- NULL
times <- c("2020-01-01 2020-02-29", "2020-03-01 2020-03-12", "2020-03-13 2020-04-12")
for (t in times)
{
  temp <- gtrends(keyword = c("coronavirus", "N-95 respirators"), geo = "US", low_search_volume=FALSE, time = t)$interest_by_region
  temp$period <- t
  coronavirus <- rbind(coronavirus, temp)
}

# Step 3 - Merge search trend data with the governor data, 
# Governor data contains party affiliation for governors of each state
governor <- read.csv(file = '~/Desktop/Covid-19 Challenge/governor_party_affiliation.csv')
coronavirus_governor <- merge(coronavirus, governor, by.x = "location", by.y = "State")

# Step 4 - Visualization
# Plot the relationship between Party_affiliation and searches for coronavirus during the three different time periods
ggplot(coronavirus_governor, aes(x=period, y=hits, fill=Party_affiliation)) + geom_boxplot() + 
  ylab("Relative Google search volume for coronavirus") + xlab("Time Period") + 
  ggtitle("Public Awareness of Coronavirus between Blue States and Red States") + theme_bw() + ylim(25, 100) + 
  facet_wrap(~period, scale="free") + scale_fill_manual(values = c("blue", "brown2")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())

# Step 5 - Statistical Test: Use Two sample t-test to comprare the difference of public awareness between blue states and red states
## t-test for Jan to Feb
coronavirus_governor_p1 <- coronavirus_governor %>% filter(period == "2020-01-01 2020-02-29")
coronavirus_governor_p1_republican <- coronavirus_governor_p1 %>% filter(Party_affiliation == "Republican")
coronavirus_governor_p1_democratic <- coronavirus_governor_p1 %>% filter(Party_affiliation == "Democratic")
var.test(coronavirus_governor_p1_republican$hits, coronavirus_governor_p1_democratic$hits, ratio = 1, alternative = "two.sided")
t.test(coronavirus_governor_p1_republican$hits, coronavirus_governor_p1_democratic$hits, alternative = "two.sided", var.equal = TRUE)

## t-test for Mar 1 - 12 - Significant
coronavirus_governor_p2 <- coronavirus_governor %>% filter(period == "2020-03-01 2020-03-12")
coronavirus_governor_p2_republican <- coronavirus_governor_p2 %>% filter(Party_affiliation == "Republican")
coronavirus_governor_p2_democratic <- coronavirus_governor_p2 %>% filter(Party_affiliation == "Democratic")
var.test(coronavirus_governor_p2_republican$hits, coronavirus_governor_p2_democratic$hits, ratio = 1, alternative = "two.sided")
t.test(coronavirus_governor_p2_republican$hits, coronavirus_governor_p2_democratic$hits, alternative = "two.sided", var.equal = TRUE)

## t-test for Mar 13 to Apr 12
coronavirus_governor_p3 <- coronavirus_governor %>% filter(period == "2020-03-13 2020-04-12")
coronavirus_governor_p3_republican <- coronavirus_governor_p3 %>% filter(Party_affiliation == "Republican")
coronavirus_governor_p3_democratic <- coronavirus_governor_p3 %>% filter(Party_affiliation == "Democratic")
var.test(coronavirus_governor_p3_republican$hits, coronavirus_governor_p3_democratic$hits, ratio = 1, alternative = "two.sided")
t.test(coronavirus_governor_p3_republican$hits, coronavirus_governor_p3_democratic$hits, alternative = "two.sided", var.equal = TRUE)

##########Conclusion############
# There is a significant difference between blue states and red states 
# in terms of goole search volumn of "Coronavirus" after coronavirus breakout (Mar 1st)
# but before President Trump decleared the national emergency (Mar 13th)

#####################################################################################################
###################Part II Relationship between Public Awarness and Interventions####################
#####################################################################################################
# Step 1 - load data
intervention <- read.csv(file = '~/Desktop/Covid-19 Challenge/Measures by state.csv')

PublicAwareness <- intervention$Hits
NumberOfInterventions <- intervention$Count_Interventions
StateOfEmergency <- intervention$State_of_emergency
SchoolRemoteOrClosure <- intervention$School_Closure_or_Remote
Cases <- intervention$case_Jan22_Mar12
Case_delta <- intervention$case_Mar1_Mar12
Tests <- intervention$Test_Mar.12
Population <- intervention$population

# Step 2 - Visualization
# Plot the realationship between interventions and partisanship
ggplot(intervention, aes(y=NumberOfInterventions, fill=Party_affiliation)) + geom_boxplot() + 
  ylab("Number of Interventions") + xlab("Blue States vs. Red States") + 
  ggtitle("Comparison of Interventions for Coronavirus") + theme_bw() + ylim(0, 13) + 
  scale_fill_manual(values = c("blue", "brown2")) + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank())

# Step 3 - select variables
step(lm(PublicAwareness~Cases+Case_delta+Tests+NumberOfInterventions+StateOfEmergency+SchoolRemoteOrClosure
        +Population),direction="both")

# Step 4 - test out the model perform
model <- lm(formula = PublicAwareness ~ Tests + NumberOfInterventions + 
              StateOfEmergency +  SchoolRemoteOrClosure + Population)
summary(model)





