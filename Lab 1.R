#Lab 1: Book Sale
#Name: Spencer Furey
#Date: September 22, 2016

#Step 1: create vectors with necessary information
title <- c("Moby Dick", "Adventures of Huckleberry Finn", "The Scarlet Letter",
         "Great Expectations", "Their Eyes Were Watching God", "The Lesser Blessed",
         "Love in the Time of Cholera", "Mrs. Dalloway", "Never Let Me Go",
         "The Short and Tragic Life of Robert Peace")
author <- c("Herman Melville", "Mark Twain", "Nathaniel Hawthorne", "Charles Dickens",
            "Zora Neale Hurston", "Richard Van Camp", "Gabriel Garcia Marquez",
            "Virginia Woolf", "Kazuo Ishiguro", "Jeff Hobbs")
published <- c(1851, 1884, 1850, 1861, 1937, 1996, 1985, 1925, 2004, 2014)
nstock <- c(15,9,7,17,15,13,6,8,3,11)
price <- c(13,11,10,14,18,16,20,17,19,21)

#Step 2: vectors have been established, now combine them into dataframe. 
bookstore <- data.frame(title, author, published, nstock, price)

#Step 3: Sort by year published in descending order, and, within year, by author in
#ascending order. 
books.sorted <- bookstore %>% arrange (-published, author)

#Step 4: Bookstore sale is happening. Conditions of the sale are as follows:
#Published <1900 are 25% off
#Copies >10 in stock are 40% off
#Published <1900 with >10 copies are 50% off

#Step 5: To represent the book sale, I need to use three ifelse statements. 
#I chose to employ the nested ifelse statement method:
sale <- bookstore %>% mutate (saleprice=ifelse(published<1900,
                                        ifelse(nstock>10,price*.5,price*.75),
                                        ifelse(nstock>10,price*.6,price)))

#Step 6: Create subset including only books on sale and only author, title, saleprice. 
salebooks <- sale %>% filter (price != saleprice) %>% select (author, title,saleprice)

#With help from Lab 1 example and Mr. James Adams. 



