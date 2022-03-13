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
