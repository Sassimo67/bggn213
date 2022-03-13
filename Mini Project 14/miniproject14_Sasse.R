vax <- read.csv("covid19vaccinesbyzipcode_test.csv")
head(vax)

#Q1: What column details the total number of people fully vaccinated?
#persons_fully_vaccinated

#Q2: What column details the Zip code tabulation area?
#zip_code_tabulation_area

#Q3: What is the earliest date in this dataset?
#2021-01-05

#Q4: What is the latest date in this dataset?
#2022-03-01



#Q5: How many numeric columns are in this dataset?
#9 columns

sum( is.na(vax$persons_fully_vaccinated) ) 

#Q6: Note that there are “missing values” in the dataset. 
#How many NA values there in the persons_fully_vaccinated column?
#18338 NAs

#Q7: What percent of persons_fully_vaccinated values are missing (to 2 significant figures)?
#17 percent


#Q9: How many days have passed since the last update of the dataset?
#5 days

#Q10: How many unique dates are in the dataset (i.e. how many different dates are detailed)?
#420

library(zipcodeR)
geocode_zip('92037')
zip_distance('92037','92109')
reverse_zipcode(c('92037', "92109") )
zipdata <- reverse_zipcode( vax$zip_code_tabulation_area )


sd <- vax[vax$county == "San Diego", ]
sd.10 <- filter(vax, vax$county == "San Diego" &
                  vax$age5_plus_population > 10000)

#Q11: How many distinct zip codes are listed for San Diego County?
#107 counties

#Q12: What San Diego County Zip code area has the largest 12 + Population in this dataset?
#92154

library(dplyr)
Q13 <- filter(vax, as_of_date == "2022-02-22")
mean(Q13$percent_of_population_fully_vaccinated, na.rm = TRUE)

#Q13: What is the overall average “Percent of Population Fully Vaccinated” 
#value for all San Diego “County” as of “2022-02-22”?
#69.7%

#Q14: Using either ggplot or base R graphics make a summary figure that shows the distribution 
#of Percent of Population Fully Vaccinated values as of “2022-02-22”?

library(ggplot2)
ggplot(Q13, aes(Q13$percent_of_population_fully_vaccinated)) +
  geom_histogram()

#Q15 and 16:

ucsd <- filter(sd, zip_code_tabulation_area=="92037")
ucsd[1,]$age5_plus_population

ggplot(ucsd) +
  aes(as_of_date, percent_of_population_fully_vaccinated) +
  geom_line() +
  geom_line(group=1) +
  ylim(c(0,1)) +
  labs(x = "Date", y="Percent Vaccinated") +
  geom_hline(yintercept = 0.732, linetype= 'dashed')

       
#16-18:

vax.36 <- filter(vax, age5_plus_population > 36144 &
                   as_of_date == "2022-02-22")
head(vax.36)

#Q19

vax %>% filter(as_of_date == "2022-02-22") %>%  
  filter(zip_code_tabulation_area=="92040") %>%
  select(percent_of_population_fully_vaccinated)

vax.36.all <- filter(vax, vax$age5_plus_population>36144)

#Q20:

ggplot(vax.36.all) +
  aes(as_of_date, percent_of_population_fully_vaccinated, group=zip_code_tabulation_area) +
  geom_line(alpha=0.2, color= 'blue') +
  ylim(0,1) +
  labs(x="Date", y="Percent Vaccinated",
       title="Vaccination rate across California",
       subtitle="Only areas with a population above 36k") +
  geom_hline(yintercept = 0.732, linetype= 'dashed')

#Q21: How do you feel about traveling for Spring Break and meeting for in-person class afterwards?
#Comfortable

sessionInfo()