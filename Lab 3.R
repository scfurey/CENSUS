#Lab 3: Industry and Occupation, 1870-1920 
#Spencer Furey

#load packages
library (dplyr)
library (readr)
library(ggplot2)
library(RColorBrewer)

#read in IPUMS data, making sure to specify the correct path
#filter out Alaska and Hawaii; limit to ages 15-65
a <- read_csv('9:29 data.csv') %>%
  filter (AGE>=15 & AGE<=65 & !(STATEFIP %in% c(2, 15)))
#check to see if it worked
a %>% filter (AGE<15 | AGE>65 | STATEFIP %in% c(2,15))


table(a$RACE)
#create race variable by recoding RACE
b <- a %>% mutate(Race=factor(ifelse(RACE>3,4,RACE),
                        labels=c('White', 'Black', 'Native American', 'Asian')))
                    

#create sex variable by recoding SEX
c <- b %>% mutate (Sex=ifelse(SEX==1, 'male','female'))

#create occupation variable by recoding OCC1950
d <- c %>% mutate(Occupation=factor(ifelse(OCC1950 %in% c(980:999), 1,
                                    ifelse(OCC1950 %in% c(100) | OCC1950 %in% c(123) | OCC1950 %in% c(810:840), 2,
                                    ifelse(OCC1950 %in% c(500:595) | OCC1950 %in% c(600:690) | OCC1950 %in% c(910:970), 3,
                                    ifelse(OCC1950 %in% c(200:290) | OCC1950 %in% c(300:390) | OCC1950 %in% c(400:490), 4,
                                    ifelse(OCC1950 %in% c(700:720) | OCC1950 %in% c(730:790), 5, 6))))),
                            labels=c('none','farmers and farm laborers',
                                     'craftsmen/operatives/laborers',
                                     'managerial/clerical/sales','service',
                                     'professional')))

#Check to see what the head (top) looks like
e <- d %>% select(YEAR, PERWT, Race, Occupation, Sex)
head (e)

#I'm going to make 2 graphs, #s 2 and 4 from class
#For the first graph, I will group by YEAR, Sex, and Race
f1 <- e %>% group_by(YEAR, Sex, Race) %>% summarise(Number=sum(PERWT))
#For the second graph, I will group by YEAR, Sex, Race, and Occupation
f2 <- e %>% group_by(YEAR, Sex, Race, Occupation) %>% summarise(Number=sum (PERWT))
head (f2)

#Graphing time! Graph #2 is up first
fig2 <- ggplot(data=f1,aes(x=YEAR, y=Number, fill=Sex)) +
  geom_bar(stat='identity') +
  labs(x='Year',y='Population',fill='Sex',title='2. Population Aged 15-65 by Race, Year, and Sex, 1870-1920') +
  scale_y_continuous(labels=scales::comma) + 
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set2',guide=guide_legend(reverse=TRUE)) +
  facet_wrap(~Race,ncol=2, scales='free_y') +
  theme_bw()
  
#Same thing for Occupation (Graph #4)
fig4 <- ggplot(data=f2,aes(x=YEAR,y=Number,fill=Occupation)) +
  geom_bar(stat='identity',position='fill') +
  labs(x='Year',y='Percent of Population',fill='Occupation',title='4. Occupation of Persons Aged 15-65 by Sex, Race, and Year, 1870-1920') +
  scale_y_continuous(labels=scales::percent) + 
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set1') +
  facet_grid(Sex~.~Race) +
  theme_bw() + theme(legend.position='bottom')

#Export figures to png
png('Figure_2.png',height=500,width=1000)
print(fig2)
dev.off()
png('Figure_4.png',height=500,width=1000)
print(fig4)
dev.off()

#With help from Drew, Mark, Kohar, and Morgan, as well as Professor Merchant
#during office hours. 
  







