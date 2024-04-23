#######################################
# QTA Final
#######################################

## Load packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

library(dplyr)
library(ggplot2)

lapply(c("tidyverse",
         "guardianapi", # for working with the Guardian's API
         "quanteda", # for QTA
         "quanteda.textstats", # more Quanteda!
         "quanteda.textplots", # even more Quanteda!
         "readtext", # for reading in text data
         "stringi", # for working with character strings
         "textstem" # an alternative method for lemmatizing
), pkgTest)



########################################
####################
####################
#   ANALYSIS OF GAZA/ISREAL CORPUS
####################
####################
########################################



### 
### Using the Guardian API with R
### 
gu_api_key() # run this interactive function
# load in the data with dates of interest
dat <- gu_content(query = "Ukraine", from_date = "2022-01")
head(dat)
data <- dat # make a duplicate

### 
### Clean up the data frame 
### 
data <- data[data$type == "article" & data$section_id == "world",]
head(data)
typeof(data)

data <- data[data$type == "article" & df$section_id == "world",] # see if you can subset the object to focus on the articles we want

which(duplicated(data$web_title) == TRUE) # sometimes there are duplicates...
data <- data[!duplicated(data$web_title),] # which we can remove
View(data)

#Tidy up our data, only keeping variables I need 
tidy_ukr <- data %>%
  select(headline,
         date = web_publication_date, # change the name of the date variable
         wordcount,
         standfirst,
         web_title,
         body_text
  ) %>% 
  mutate(date = as_datetime(date))

tidy_ukr$date <- as.Date(tidy_ukr$date)
# Check if its changed to date time
class(tidy_ukr$date)

print(tidy_ukr$date[1])

View(tidy_ukr)                 
summary(tid_ukr,5)

#lets check the year of 2023
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")
ukr_filtered <- tidy_ukr[tidy_ukr$date >= start_date & tidy_ukr$date <= end_date, ]
View(ukr_filtered)

### 
### Make a corpus 
### 

#the docvars to be included
ukr_docvars <- list(date = tidy_ukr$date,
                    word_count = tidy_ukr$wordcount)

#create corpus
corp_ukr <- corpus(x = tidy_ukr,
                   docid_field = "web_title",
                   text_field = "headline",
                   docvars = ukr_docvars)

# Lets check out some of these documents!!!
summary(corp_ukr,5) 
docvars(corp_ukr[2])
print(corp_ukr[7])
print(corp_ukr[35])

ukr_corp_sum <- summary(corp_ukr,
                        n = nrows(docvars(corp_ukr))) 
print(ukr_corp_sum)



###
# CHECK THE NUMBER OF ARTICLES WRITTEN
###

# Convert datetime to date
corp_ukr$date <- as.Date(corp_ukr$date)

# Filter corpus for the desired range of time
start_date <- as.Date("2023-01-01")
end_date <- as.Date("2023-12-31")
corp_filtered <- corp_ukr[corp_ukr$date >= start_date & corp_ukr$date <= end_date, ]

# Count articles per day
article_counts <- table(corp_filtered$date)

# Print the counts
print(article_counts)

# Count the occurrences of each date
date_counts <- table(corp_filtered$date)

# Plot the number of observations for each date
# based on the plot there seems to be no drop in the number of articles about Ukraine
plot(date_counts, 
        main = "Number of Observations by Date",
        xlab = "Date",
        ylab = "Number of Observations",
        col = "skyblue",
        las = 2)  # Rotate x-axis labels vertically

###
# CHECK FOR DIFFERENCE IN MEANS
###
# Subset the data for 5 months before and 5 months afer the attack
five_before <- as.Date("2023-05-07")
five_after <- as.Date("2024-02-07")
ten_months_df <- tidy_ukr[tidy_ukr$date >= five_before & tidy_ukr$date <= five_after, ]

#order by date
ten_months_df <- ten_months_df[order(ten_months_df$date),]
# check to ensure we have relivant dates
max(ten_months_df$date)
min(ten_months_df$date)
View(ten_months_df)
# This is the date of the hamas attack on the music festival
specified_date <- as.Date("2023-10-07")

# Calculate the daily counts of articles before and after the specified date
daily_counts_before <- table(as.Date(ten_months_df$date[ten_months_df$date < specified_date]))
daily_counts_after <- table(as.Date(ten_months_df$date[ten_months_df$date >= specified_date]))
# Print the daily counts
print(daily_counts_before)
print(daily_counts_after)

####### Preform Test ############
# Print the results
install.packages("BSDA")
library(BSDA)
# Perform z-test for difference in means
# Perform z-test for difference in means
z_test_result <- z.test(x = daily_counts_before,
                        y = daily_counts_after,
                        alternative = "two.sided",
                        sigma.x = sd(daily_counts_before),
                        sigma.y = sd(daily_counts_after))

?z.test
# Print the results
print(z_test_result)

##################################
# Subset the data for 2 months before and 2 months afer the attack
two_before <- as.Date("2023-08-07")
two_after <- as.Date("2023-12-07")
two_month_df <- tidy_ukr[tidy_ukr$date >= one_before & tidy_ukr$date <= two_after, ]

#order by date
two_month_df <- two_month_df[order(two_month_df$date),]
# check to ensure we have relivant dates
max(two_month_df$date)
min(two_month_df$date)

# Calculate the daily counts of articles before and after the specified date
two_daily_counts_before <- table(as.Date(two_month_df$date[two_month_df$date < specified_date]))
two_daily_counts_after <- table(as.Date(two_month_df$date[two_month_df$date >= specified_date]))

####### Preform Test ############
two_z_test_result <- z.test(x = two_daily_counts_before,
                        y = one_daily_counts_after,
                        alternative = "two.sided",
                        sigma.x = sd(two_daily_counts_before),
                        sigma.y = sd(two_daily_counts_after))

# Print the results
print(two_z_test_result)




########################################
####################
####################
#   ANALYSIS OF UKRAINE CORPUS
####################
####################
########################################
##SUBSET FIRST MONTH OF DATA
first_month_ukr <- subset(tidy_ukr, date <= as.Date("2022-02-24")  & date <= "2022-03-24")
View(first_month_ukr)
length(first_month_ukr$headline)
#check for dupes 
which(duplicated(first_month_ukr$headline))
#save it to my computer
write.csv(first_month_ukr, "/Users/riccimason99/Downloads/first_month_ukr.csv", row.names = FALSE)
###
#MAKE CORPUS
###

#the docvars to be included
ukr_docvars <- list(date = tidy_ukr$date,
                    word_count = tidy_ukr$wordcount)


#create corpus
first_corp_ukr <- corpus(x = first_month_ukr,
                         docid_field = "web_title",
                         text_field = "body",
                         docvars = ukr_docvars)
as.character(first_corp_ukr[[1]])


first_corp_ukr_sum <- summary(first_corp_ukr)
dim(dfm_ukr)

###
# TOKENIZE
###
token <- quanteda::tokens(first_corp_ukr, 
                          remove_punct = TRUE, 
                          remove_symbols = TRUE,
                          remove_url = TRUE)

is.tokens(token)
# Lowercase the text
token <- tokens_tolower(token)

print(token[10]) # print lowercase tokens from the 10th article in corpus.

stop_list <- stopwords("english") # load English stopwords from quanteda
super_stop <- c("p", "h2", "href","a", "said")
stop_list <- c(stop_list, super_stop)
head(stop_list)                   # show first 6 stopwords from stopword list.  
print(stop_list)
# The tokens_remove() function allows us to apply the stop_list to our toks object
token <- tokens_remove(token, stop_list)
print(token[10])

#stemming
stem_toks <- tokens_wordstem(token)
#check it 
print(token[10]) 
print(stem_toks[10]) 

#collocations
collocations <- textstat_collocations(stem_toks, size = 2)
#View(collocations)
# ii. Choose which to keep
keep_coll_list <- collocations$collocation[1:20]
keep_coll_list
comp_tok <- tokens_compound(stem_toks, keep_coll_list)

###
# Convert to dfm
###
dfm_ukr <- dfm(comp_tok)
topfeatures(dfm_ukr)


####### Sentiment Analysis

# Load the built-in sentiment dictionary
data("data_dictionary_LSD2015")

dfm_sentiment <- dfm_lookup(dfm_ukr, data_dictionary_LSD2015[1:2])
dfm_sentiment

neg <- docvars(dfm_sentiment, "prop_negative") <- as.numeric(dfm_sentiment[,1] / ntoken(dfm_sentiment))
pos <- docvars(dfm_sentiment, "prop_positive") <- as.numeric(dfm_sentiment[,2] / ntoken(dfm_sentiment))

net_sent <- (sum(pos)-sum(neg))
net_sent






