library(dplyr)
# Lab 1
#2 Date: September 21 2016 
# Step 1: Create vectors with necessary information. 
title <- c("Moby Dick", "Adventures of Huckleberry Finn", "Scarlet Letter", 
           "Great Expectations", "Their Eyes Were Watching God", 
           "The Lesser Blessed","Love in the Time of Cholera", "Mrs. Dalloway",
           "Never Let Me Go","The Short and Tragic Life of Robert Peace")
author <- c("Herman Melville", "Mark Twain", "Nathaniel Hawthorne","Charles Dickens", "Zora Neale Hursten", "Richard Van Camp","Gabriel Garcia Marquez", "Virginia Woolf", "Kazuo Ishiguro","Jeff Hobbs")
published <- c(1851, 1884, 1850, 1861, 1937, 1996, 1985, 1925, 2004, 2014) 
nstock <- c(15, 9, 7, 17, 15, 13, 6, 8, 3, 11)
price <- c(13, 11, 10, 14, 18, 16, 20, 17, 19, 21)

#create shodataframe bookstore inventory
bookstore <- data.frame (title, author, published, nstock, price)

books.sorted <- bookstore %>% arrange (-published, author)

#Bookstore sale- see below for terms
#published <1900 25% off
#>10 copies are 40% off
#published <1900 with 10+ copies are 50% off

#sequential ifelse statements
sale <- bookstore %>% mutate(saleprice=ifelse(published<1900,price*.75,price)) %>%
                      mutate (saleprice=ifelse(nstock>10,price*.6,saleprice)) %>%
                      mutate (saleprice=ifelse (published<1900 & nstock.10, price*.5, 
                                                saleprice))
#finally, create subset including only books on sale and only author, title, saleprice.
salebooks <- sale %>% filter (price != saleprice) %>% select (author, title, saleprice)

show(title)
