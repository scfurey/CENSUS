#9/22/2016 Lab 2
#Spencer Furey 


#Invoke packages; need dplyr, readr, and tidyr. I had to install readr and tidyr because
#I have not used them previously. 
library (dplyr)
library (readr)
library (tidyr)

#Reading in IPUMS data from the codebook, which is called "lab 2"and in csv format 
#Exclude Alaska and Hawaii
a <- read_csv('lab2.csv') %>% filter(!(YEAR < 1960 & STATEFIP %in% c(2, 15)))
#look at head (top) and tail (bottom) of the dataset
head (a)
tail (a)

#Read in crosswalk, creating a dataframe titled "RACES" 
#Import races.csv file from data folder, containing relevant racial classifications. 
RACES <- read_csv('Races.csv', col_types = cols(RACED="i"))

#Left join a and races by RACED
c <- left_join(a,RACES,by='RACED')
#Look at head (top) and tail (bottom) of dataset
head(c)
tail (c)

#Use group_by() and summarise () to determine population by year
g <- c %>% group_by(YEAR) %>% summarise(NUMBER = sum (PERWT))
#Now that we have this data, we want population by year and Race, use group and 
#summarise again
h <- c %>% group_by(YEAR, Race) %>% summarise (NUMBER = sum (PERWT))
#Use spread function to form one row for each race and one column for each year 
j <- h %>% spread(YEAR, NUMBER)



#Export to csv
write_csv(j,'year_RACED.csv')

#With help from Robert and Kohar, as well as Professor Merchant during office hours. 

